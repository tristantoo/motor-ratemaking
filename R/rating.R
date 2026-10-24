# Pure functions for the rating plan. Tested in tests/testthat/test_rating.R.
library(data.table)

band <- function(d) {
  d[, `:=`(
    VehPowerB   = factor(pmin(VehPower, 9)),
    VehAgeB     = cut(VehAge, c(-1, 0, 2, 5, 10, 15, Inf), labels = c("0", "1-2", "3-5", "6-10", "11-15", "16+")),
    DrivAgeB    = cut(DrivAge, c(17, 20, 25, 30, 40, 50, 60, 70, Inf),
                      labels = c("18-20", "21-25", "26-30", "31-40", "41-50", "51-60", "61-70", "71+")),
    BonusMalusB = cut(pmin(BonusMalus, 150), c(49, 50, 60, 70, 80, 90, 100, Inf),
                      labels = c("50", "51-60", "61-70", "71-80", "81-90", "91-100", "101+")),
    VehGas      = factor(VehGas),
    Area        = factor(Area), VehBrand = factor(VehBrand), Region = factor(Region)
  )]
  d
}

# Reference level of each rating variable = the level with the most exposure in `ref`.
base_levels <- function(ref, vars) {
  sapply(vars, function(v) { e <- ref[, .(e = sum(Exposure)), by = v]; as.character(e[[v]][which.max(e$e)]) })
}
set_base <- function(d, bases) {
  for (v in names(bases)) set(d, j = v, value = relevel(factor(d[[v]]), ref = bases[[v]]))
  d
}

# GLM relativities for one factor, normalized so the base level is exactly 1.
relativities <- function(fit, var, data, base) {
  levs <- levels(data[[var]])
  co <- coef(fit)[paste0(var, levs)]; co[is.na(co)] <- 0
  setNames(exp(co - co[levs == base]), levs)
}

# Observed one-way relativity: frequency (or loss cost) per exposure, divided by the base level's.
one_way <- function(d, var, y, base) {
  r <- d[, .(v = sum(get(y)) / sum(Exposure)), by = var]
  setNames(r$v / r$v[r[[var]] == base], as.character(r[[var]]))
}

# Limited fluctuation credibility: full standard 1,082 claims (p = 0.90, k = 0.05).
credibility_z <- function(n, full = 1082) pmin(1, sqrt(n / full))

# Limit each level's change against current to +/- cap; returns final relativity and a capped flag.
cap_change <- function(proposed, current, cap = 0.15) {
  ratio <- proposed / current
  list(final = current * pmin(pmax(ratio, 1 - cap), 1 + cap), capped = ratio < 1 - cap | ratio > 1 + cap)
}

# Loss ratio method: projected loss and LAE ratio over permissible loss ratio, minus 1.
indication <- function(loss, lae_load, trend, years, ldf, premium, var_exp, profit) {
  proj_lr <- loss * ldf * (1 + trend)^years * (1 + lae_load) / premium
  proj_lr / (1 - var_exp - profit) - 1
}

exposure_bins <- function(order_by, expo, n) {
  o <- order(order_by)
  bin <- integer(length(o))
  bin[o] <- cut(cumsum(expo[o]) / sum(expo), seq(0, 1, length.out = n + 1), labels = FALSE, include.lowest = TRUE)
  bin
}

# pred and actual are totals per policy (not rates); bins hold equal exposure.
lift_table <- function(pred, actual, expo, n = 10) {
  d <- data.table(pred, actual, expo, bin = exposure_bins(pred / expo, expo, n))
  d[, .(pred = sum(pred) / sum(expo), actual = sum(actual) / sum(expo), expo = sum(expo)), by = bin][order(bin)]
}

# Sort by model disagreement; the better model has the flatter actual/predicted ratio across bins.
double_lift <- function(p_a, p_b, actual, expo, n = 10) {
  d <- data.table(p_a, p_b, actual, expo, bin = exposure_bins(p_b / p_a, expo, n))
  d[, .(ratio_b_to_a = sum(p_b) / sum(p_a), ae_a = sum(actual) / sum(p_a), ae_b = sum(actual) / sum(p_b),
        expo = sum(expo)), by = bin][order(bin)]
}

# Lorenz curve ordered by predicted rate (ascending, ties pooled); Gini = 1 - 2 * area under it.
lorenz <- function(pred_rate, actual, expo) {
  g <- data.table(p = pred_rate, a = actual, e = expo)[, .(a = sum(a), e = sum(e)), by = p][order(p)]
  data.table(x = c(0, cumsum(g$e) / sum(g$e)), y = c(0, cumsum(g$a) / sum(g$a)))
}
gini <- function(pred_rate, actual, expo) {
  l <- lorenz(pred_rate, actual, expo)
  1 - 2 * sum(diff(l$x) * (head(l$y, -1) + tail(l$y, -1)) / 2)
}

pois_dev  <- function(y, mu) 2 * (ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
gamma_dev <- function(y, mu, w = 1) 2 * w * (-log(y / mu) + (y - mu) / mu)

# Paired bootstrap over policies: percent reduction in total deviance of b against reference a.
paired_boot <- function(dev_a, dev_b, B = 1000, seed = 2026) {
  set.seed(seed); n <- length(dev_a)
  pct <- function(i) 100 * (sum(dev_a[i]) - sum(dev_b[i])) / sum(dev_a[i])
  draws <- vapply(seq_len(B), function(b) pct(sample.int(n, n, replace = TRUE)), numeric(1))
  list(pct_reduction = pct(seq_len(n)), lo95 = unname(quantile(draws, 0.025)), hi95 = unname(quantile(draws, 0.975)), B = B)
}
