# End to end: data prep, GLMs, rating plan, indication, lift, GBM challenger, bootstrap, mock filing.
# Usage: Rscript R/download.R && Rscript run.R
suppressPackageStartupMessages({ library(data.table); library(xgboost); library(jsonlite) })
source("R/rating.R"); source("R/exhibits.R")
dir.create("outputs", showWarnings = FALSE)
B <- 1000

# ---- assumptions (illustrative except where noted) ----
A <- list(ldf = 1.00, trend = 0.03, years = 2, ulae = 0.10, var_exp = 0.25, profit = 0.05, cap_q = 0.995, cred_full = 1082, change_cap = 0.15)

# ---- data ----
f <- as.data.table(readRDS("data/raw/freq.rds")); s <- as.data.table(readRDS("data/raw/sev.rds"))
n_raw <- nrow(f)
gap <- f[IDpol <= 24500, .(policies = .N, claims_in_freq = sum(ClaimNb))]
gap$sev_records <- s[IDpol <= 24500, .N]
f <- f[IDpol > 24500]                                   # documented choice: drop the block without severity records
s <- s[IDpol %in% f$IDpol]
nsev <- s[, .(n_sev = .N), by = IDpol]
f[nsev, n_sev := i.n_sev, on = "IDpol"][is.na(n_sev), n_sev := 0L]
n_count_fixed <- f[ClaimNb != n_sev, .N]
f[, `:=`(ClaimNb = n_sev, n_sev = NULL, Exposure = pmin(Exposure, 1))]   # counts taken from the severity file

set.seed(2026)
f[, u := runif(.N)][, test := frank(u, ties.method = "first") <= round(0.2 * .N), by = pmin(ClaimNb, 2)][, u := NULL]
s[f, test := i.test, on = "IDpol"]
cap_amt <- unname(quantile(s[test == FALSE, ClaimAmount], A$cap_q))
s[, capped := pmin(ClaimAmount, cap_amt)]
loss <- s[, .(loss = sum(ClaimAmount), loss_c = sum(capped)), by = IDpol]
f[loss, `:=`(loss = i.loss, loss_c = i.loss_c), on = "IDpol"][is.na(loss), `:=`(loss = 0, loss_c = 0)]
xs <- s[test == FALSE, sum(ClaimAmount) / sum(capped)]   # excess loss factor spreads the capped excess back

vars <- c("VehPowerB", "VehAgeB", "DrivAgeB", "BonusMalusB", "VehBrand", "VehGas", "Area", "Region")
cur_vars <- c("BonusMalusB", "DrivAgeB", "Region")
sev_vars <- c("DrivAgeB", "VehAgeB", "BonusMalusB", "VehGas")
band(f)
bases <- base_levels(f[test == FALSE], vars)
f <- set_base(f, bases)
tr <- f[test == FALSE]; te <- f[test == TRUE]
cat(sprintf("policies %d (raw %d), train %d, test %d, claims %d, cap %.0f, excess factor %.4f\n",
            nrow(f), n_raw, nrow(tr), nrow(te), sum(f$ClaimNb), cap_amt, xs))

# ---- GLMs ----
fml <- function(lhs, v, off = TRUE) as.formula(paste(lhs, "~", paste(c(v, if (off) "offset(log(Exposure))"), collapse = " + ")))
freq_cur  <- glm(fml("ClaimNb", cur_vars), family = poisson, data = tr)
freq_prop <- glm(fml("ClaimNb", vars), family = poisson, data = tr)
dispersion <- sum(residuals(freq_prop, type = "pearson")^2) / df.residual(freq_prop)
nb <- MASS::glm.nb(fml("ClaimNb", vars), data = tr)
nb_check <- list(pearson_dispersion_poisson = dispersion, nb_theta = nb$theta, aic_poisson = AIC(freq_prop), aic_nb = AIC(nb),
                 max_abs_rel_coef_gap = max(abs(exp(coef(nb) - coef(freq_prop)) - 1), na.rm = TRUE))
tr_c <- tr[ClaimNb > 0][, avg_c := loss_c / ClaimNb]; te_c <- te[ClaimNb > 0][, avg_c := loss_c / ClaimNb]
sev <- glm(fml("avg_c", sev_vars, FALSE), family = Gamma(link = "log"), weights = ClaimNb, data = tr_c)
sev_mean <- tr_c[, sum(loss_c) / sum(ClaimNb)]
# Severity relativities enter the plan only if the Gamma GLM beats a flat mean on the holdout (a test set decision, disclosed).
sev_boot <- paired_boot(gamma_dev(te_c$avg_c, sev_mean, te_c$ClaimNb),
                        gamma_dev(te_c$avg_c, predict(sev, te_c, type = "response"), te_c$ClaimNb), B)
sev_ok <- sev_boot$lo95 > 0
# Diagnostic only: one interaction chosen a priori (young drivers carry high bonus malus), checked once against the GBM gap.
freq_int <- glm(fml("ClaimNb", c(vars, "DrivAgeB:BonusMalusB")), family = poisson, data = tr)

freq_rate <- tr[, sum(ClaimNb) / sum(Exposure)]
add_preds <- function(d) {
  d[, `:=`(mu0 = Exposure * freq_rate, mu_cur = predict(freq_cur, d, type = "response"),
           mu_prop = predict(freq_prop, d, type = "response"), sev_prop = if (sev_ok) predict(sev, d, type = "response") else sev_mean)]
  d[, `:=`(pp_cur = mu_cur * sev_mean, pp_prop = mu_prop * sev_prop)]
}
add_preds(tr); add_preds(te)
te[, mu_int := predict(freq_int, te, type = "response")]

# ---- GBM challenger (frequency), exposure offset via base_margin ----
X <- function(d) Matrix::sparse.model.matrix(~ VehPower + VehAge + DrivAge + BonusMalus + VehGas + log(Density) + Area + VehBrand + Region - 1, d)
set.seed(2027)
tr[, val := frank(runif(.N), ties.method = "first") <= round(0.1 * .N), by = pmin(ClaimNb, 2)]
dm <- function(d) xgb.DMatrix(X(d), label = d$ClaimNb, base_margin = log(d$Exposure))
dfit <- dm(tr[val == FALSE]); dval <- dm(tr[val == TRUE]); dte <- dm(te)
params <- list(objective = "count:poisson", eta = 0.05, max_depth = 4, min_child_weight = 50, subsample = 0.8,
               tree_method = "hist", nthread = 4, seed = 2026, eval_metric = "poisson-nloglik")
gbm <- xgb.train(params, dfit, nrounds = 2000, evals = list(val = dval), early_stopping_rounds = 50, verbose = 0)
best_iter <- xgb.attr(gbm, "best_iteration")
te[, mu_gbm := predict(gbm, dte)]
stopifnot(abs(sum(te$mu_gbm) / sum(te$ClaimNb) - 1) < 0.1)   # base_margin really applied

# ---- holdout comparison with paired bootstrap ----
dv <- function(mu) pois_dev(te$ClaimNb, mu)
cmp <- list(
  proposed_glm_vs_exposure_only = paired_boot(dv(te$mu0), dv(te$mu_prop), B),
  proposed_glm_vs_current_glm   = paired_boot(dv(te$mu_cur), dv(te$mu_prop), B),
  gbm_vs_proposed_glm           = paired_boot(dv(te$mu_prop), dv(te$mu_gbm), B),
  glm_interaction_vs_proposed   = paired_boot(dv(te$mu_prop), dv(te$mu_int), B),
  gbm_vs_glm_interaction        = paired_boot(dv(te$mu_int), dv(te$mu_gbm), B),
  severity_glm_vs_mean          = sev_boot)
model_table <- rbindlist(lapply(names(cmp), function(k) data.table(comparison = k,
  pct_deviance_reduction = round(cmp[[k]]$pct_reduction, 3), lo95 = round(cmp[[k]]$lo95, 3), hi95 = round(cmp[[k]]$hi95, 3))))
gini_table <- data.table(
  model = c("current GLM (frequency)", "proposed GLM (frequency)", "GBM (frequency)", "current plan (pure premium)", "proposed GLM (pure premium)"),
  gini = round(c(te[, gini(mu_cur / Exposure, ClaimNb, Exposure)], te[, gini(mu_prop / Exposure, ClaimNb, Exposure)],
                 te[, gini(mu_gbm / Exposure, ClaimNb, Exposure)], te[, gini(pp_cur / Exposure, loss_c, Exposure)],
                 te[, gini(pp_prop / Exposure, loss_c, Exposure)]), 4))
print(model_table); print(gini_table)

# ---- relativities, credibility, caps ----
rel <- rbindlist(lapply(vars, function(v) {
  b <- bases[[v]]; levs <- levels(tr[[v]])
  st <- tr[, .(exposure = sum(Exposure), claims = sum(ClaimNb)), by = v]; setnames(st, v, "level")
  fr <- relativities(freq_prop, v, tr, b)
  sr <- if (v %in% sev_vars && sev_ok) relativities(sev, v, tr, b) else setNames(rep(1, length(levs)), levs)
  cr <- if (v %in% cur_vars) relativities(freq_cur, v, tr, b) else setNames(rep(1, length(levs)), levs)
  ow <- one_way(tr, v, "ClaimNb", b)
  d <- data.table(var = v, level = levs, base = levs == b, one_way_freq = ow[levs], glm_freq = fr[levs], glm_sev = sr[levs],
                  indicated = fr[levs] * sr[levs], current = cr[levs])
  st[, level := as.character(level)][d, on = "level"][, c("var", "level", "base", "exposure", "claims", "one_way_freq",
                                                          "glm_freq", "glm_sev", "indicated", "current"), with = FALSE]
}))
rel[, Z := credibility_z(claims, A$cred_full)][, cred := Z * indicated + (1 - Z) * current]
rel[, c("final", "capped") := cap_change(cred, current, A$change_cap)]
rel[, change_pct := 100 * (final / current - 1)]

# ---- plan premiums over all policies, indication, dislocation ----
f[, mu_cur := predict(freq_cur, f, type = "response")]
f[, cur_premium := mu_cur * sev_mean * xs * (1 + A$ulae) / (1 - A$var_exp - A$profit)]  # incumbent: experience period cost, no trend
f[, prodrel := 1]
for (v in vars) { r <- rel[var == v]; f[, prodrel := prodrel * r$final[match(as.character(get(v)), r$level)]] }
base_rate <- f[, sum(cur_premium) / sum(prodrel * Exposure)]          # off-balance: proposed structure is revenue neutral
f[, rn_premium := base_rate * prodrel * Exposure]
ind <- indication(loss = f[, sum(loss_c)] * xs, lae_load = A$ulae, trend = A$trend, years = A$years, ldf = A$ldf,
                  premium = f[, sum(cur_premium)], var_exp = A$var_exp, profit = A$profit)
f[, `:=`(prop_premium = rn_premium * (1 + ind), chg_rn = rn_premium / cur_premium - 1)]
disloc_table <- data.table(band = c("below -10%", "-10% to +10%", "above +10%"),
  share_of_policies = round(c(f[, mean(chg_rn < -0.10)], f[, mean(abs(chg_rn) <= 0.10)], f[, mean(chg_rn > 0.10)]), 4))
q <- quantile(f$chg_rn, c(0.01, 0.10, 0.5, 0.90, 0.99))

# ---- lift ----
lift <- te[, lift_table(pp_prop, loss_c, Exposure)]
dl_cp <- te[, double_lift(pp_cur, pp_prop, loss_c, Exposure)]
dl_gg <- te[, double_lift(mu_prop, mu_gbm, ClaimNb, Exposure)]

# ---- outputs ----
assumptions <- data.table(
  assumption = c("Experience losses", "Exposure", "Large loss cap", "Excess loss factor", "Loss development factor", "Annual loss trend",
                 "ULAE load", "Variable expense ratio", "Profit and contingency", "Current premium"),
  value = c(sprintf("%.0f", f[, sum(loss_c)] * xs), sprintf("%.0f", sum(f$Exposure)), sprintf("%.0f per claim", cap_amt), sprintf("%.4f", xs),
            sprintf("%.2f", A$ldf), sprintf("%.0f%% for %d years", 100 * A$trend, A$years), sprintf("%.0f%% of loss", 100 * A$ulae),
            sprintf("%.0f%%", 100 * A$var_exp), sprintf("%.0f%%", 100 * A$profit), sprintf("%.0f", f[, sum(cur_premium)])),
  basis = c("estimated: capped losses x excess factor", "data", "estimated: 99.5th pct of train claims", "estimated: train",
            "assumed (data treated as ultimate)", "illustrative", "illustrative", "illustrative", "illustrative",
            "assumed: incumbent plan priced at experience period cost, no trend"))
metrics <- list(
  data = list(policies_raw = n_raw, idpol_le_24500_dropped = gap, policies = nrow(f), train = nrow(tr), test = nrow(te),
              claims = sum(f$ClaimNb), counts_replaced_from_severity_file = n_count_fixed, exposure_years = sum(f$Exposure),
              large_loss_cap = cap_amt, excess_factor = xs, split = "random 80/20 stratified by claim count (0, 1, 2+), seed 2026; NOT forward chaining"),
  overdispersion = nb_check, severity_relativities_used = sev_ok, holdout = cmp, gini = gini_table, gbm = list(best_iteration = best_iter, params = params),
  indication = list(indicated_change = ind, assumptions = A), dislocation = list(table = disloc_table, quantiles = as.list(q)),
  capped_cells = rel[capped == TRUE, .N], cells = nrow(rel))
write_json(metrics, "outputs/metrics.json", auto_unbox = TRUE, pretty = TRUE, digits = 6)
fwrite(rel, "outputs/relativities.csv"); fwrite(rel[capped == TRUE], "outputs/capped_cells.csv")
fwrite(assumptions, "outputs/assumptions.csv"); fwrite(model_table, "outputs/holdout_bootstrap.csv"); fwrite(gini_table, "outputs/gini.csv")
fwrite(lift, "outputs/lift.csv"); fwrite(dl_cp, "outputs/double_lift_current_vs_proposed.csv"); fwrite(dl_gg, "outputs/double_lift_glm_vs_gbm.csv")
fwrite(disloc_table, "outputs/dislocation.csv")
coef_tab <- function(m) { co <- summary(m)$coefficients; data.table(term = rownames(co), estimate = round(co[, 1], 4), se = round(co[, 2], 4), relativity = round(exp(co[, 1]), 4)) }
fwrite(coef_tab(freq_prop), "outputs/glm_frequency_coefficients.csv"); fwrite(coef_tab(sev), "outputs/glm_severity_coefficients.csv")

png("outputs/oneway_vs_glm.png", 1400, 750, res = 110); plot_oneway(rel, vars); dev.off()
png("outputs/lift.png", 900, 600, res = 110); plot_lift(lift); dev.off()
png("outputs/double_lift.png", 1400, 600, res = 110); par(mfrow = c(1, 2))
plot_double(dl_cp, "current", "proposed", "Pure premium: current vs proposed"); plot_double(dl_gg, "GLM", "GBM", "Frequency: GLM vs GBM"); dev.off()
curves <- list(current = te[, lorenz(mu_cur / Exposure, ClaimNb, Exposure)], `proposed GLM` = te[, lorenz(mu_prop / Exposure, ClaimNb, Exposure)],
               GBM = te[, lorenz(mu_gbm / Exposure, ClaimNb, Exposure)])
png("outputs/lorenz.png", 900, 700, res = 110); plot_lorenz(curves, gini_table$gini[1:3]); dev.off()
png("outputs/dislocation.png", 900, 600, res = 110); plot_disloc(f$chg_rn); dev.off()

pct <- function(k) sprintf("%.2f%% (95%% CI %.2f%% to %.2f%%)", cmp[[k]]$pct_reduction, cmp[[k]]$lo95, cmp[[k]]$hi95)
make_filing("outputs/mock_filing.pdf", list(
  summary_lines = c(sprintf("Policies: %s; claims: %s; exposure years: %.0f.", format(nrow(f), big.mark = ","), format(sum(f$ClaimNb), big.mark = ","), sum(f$Exposure)),
                    sprintf("Overall indicated change: %+.1f%%.", 100 * ind),
                    sprintf("Severity relativities used: %s (Gamma GLM vs flat mean on holdout: %s).", sev_ok, pct("severity_glm_vs_mean")),
                    sprintf("Policies whose premium moves more than 10%% either way (revenue neutral structure): %.1f%%.", 100 * f[, mean(abs(chg_rn) > 0.10)])),
  assumptions = assumptions,
  indication_lines = c(sprintf("Projected loss and LAE ratio = losses x LDF x (1 + trend)^years x (1 + ULAE) / premium = %.4f",
                               (ind + 1) * (1 - A$var_exp - A$profit)),
                       sprintf("Permissible loss ratio = 1 - %.2f - %.2f = %.2f", A$var_exp, A$profit, 1 - A$var_exp - A$profit),
                       sprintf("Indicated overall change = %.4f / %.2f - 1 = %+.2f%%", (ind + 1) * (1 - A$var_exp - A$profit), 1 - A$var_exp - A$profit, 100 * ind),
                       "", "The current premium is the incumbent plan priced at experience period cost, so the indication is",
                       "driven by the trend assumption plus any gap between incumbent expected and actual losses."),
  rel_exhibit = rel[, .(var, level, exposure = round(exposure), claims, indicated = round(indicated, 3), current = round(current, 3),
                        Z = round(Z, 2), cred = round(cred, 3), final = round(final, 3), change_pct = round(change_pct, 1), capped)],
  disloc_table = disloc_table, disloc = f$chg_rn,
  disloc_lines = c("Quantiles of proposed / current - 1 (revenue neutral structure):", paste(names(q), sprintf("%+.1f%%", 100 * q), collapse = "   "),
                   sprintf("The overall %+.1f%% change applies on top of these.", 100 * ind)),
  model_table = model_table, gini_table = gini_table, lift = lift, dl_cp = dl_cp, rel = rel, vars = vars,
  coef_freq = coef_tab(freq_prop), coef_sev = coef_tab(sev)))
cat("indication", round(100 * ind, 2), "% | GBM best iteration", best_iter, "| capped cells", rel[capped == TRUE, .N], "of", nrow(rel), "\n")
cat("wrote outputs/\n")
