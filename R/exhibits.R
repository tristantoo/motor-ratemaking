# Charts (PNG and filing pages) and the mock filing PDF, all base R graphics.
FOOTER <- "MOCK FILING. Educational exercise on public data. Not an actual rate filing."

plot_oneway <- function(rel, vars) {
  op <- par(mfrow = c(2, 4), mar = c(5, 3, 2.5, 0.5), oma = c(0, 0, 2, 0)); on.exit(par(op))
  for (v in vars) {
    r <- rel[var == v]; x <- seq_len(nrow(r)); yl <- range(c(r$one_way_freq, r$glm_freq), na.rm = TRUE)
    plot(x, r$one_way_freq, type = "b", pch = 1, ylim = yl, xaxt = "n", xlab = "", ylab = "", main = v, col = "grey40")
    lines(x, r$glm_freq, type = "b", pch = 19, col = "#1f5fa8")
    axis(1, at = x, labels = r$level, las = 2, cex.axis = 0.7); abline(h = 1, lty = 3)
  }
  mtext("Frequency relativity: one-way (open, grey) vs GLM (filled, blue); base level = 1", outer = TRUE, cex = 0.9)
}

plot_lift <- function(lt) {
  yl <- range(c(lt$pred, lt$actual))
  plot(lt$bin, lt$actual, type = "b", pch = 19, ylim = yl, xlab = "Decile of predicted loss cost (equal exposure)",
       ylab = "Capped loss per exposure year", main = "Lift, proposed plan, test set")
  lines(lt$bin, lt$pred, type = "b", pch = 1, lty = 2)
  legend("topleft", c("actual", "predicted"), pch = c(19, 1), lty = c(1, 2), bty = "n")
}

plot_double <- function(dl, name_a, name_b, main) {
  yl <- range(c(dl$ae_a, dl$ae_b))
  plot(dl$bin, dl$ae_a, type = "b", pch = 1, ylim = yl, xlab = paste0("Decile of ", name_b, " / ", name_a),
       ylab = "Actual / predicted", main = main, col = "grey40")
  lines(dl$bin, dl$ae_b, type = "b", pch = 19, col = "#1f5fa8"); abline(h = 1, lty = 3)
  legend("topleft", c(name_a, name_b), pch = c(1, 19), col = c("grey40", "#1f5fa8"), bty = "n")
}

plot_lorenz <- function(curves, ginis) {
  plot(0:1, 0:1, type = "l", lty = 3, xlab = "Cumulative exposure (sorted by predicted frequency)",
       ylab = "Cumulative claims", main = "Lorenz curves, test set")
  cols <- c("grey40", "#1f5fa8", "#c0392b")
  for (i in seq_along(curves)) lines(curves[[i]]$x, curves[[i]]$y, col = cols[i], lwd = 2)
  legend("topleft", sprintf("%s (Gini %.3f)", names(curves), ginis), col = cols, lwd = 2, bty = "n")
}

plot_disloc <- function(chg) {
  hist(pmin(pmax(chg, -0.6), 0.6) * 100, breaks = seq(-60, 60, 2.5), main = "Premium change, revenue neutral structure",
       xlab = "Proposed vs current premium, % (clipped at +/- 60)", col = "#9db7d8", border = "white")
  abline(v = c(-10, 10), lty = 2)
}

# ---- mock filing PDF ----
page <- function(title) {
  par(mfrow = c(1, 1), mar = c(1, 1, 3, 1), oma = c(2, 0, 0, 0)); plot.new()
  title(title, cex.main = 1.1); mtext(FOOTER, side = 1, outer = TRUE, cex = 0.7, line = 0.5)
}
text_page <- function(title, lines, cex = 0.62, per = 64) {
  for (chunk in split(lines, ceiling(seq_along(lines) / per))) {
    page(title); text(0, 1, paste(chunk, collapse = "\n"), adj = c(0, 1), family = "mono", cex = cex)
  }
}
tbl <- function(df) { o <- options(width = 250); on.exit(options(o)); capture.output(print(as.data.frame(df), row.names = FALSE, right = FALSE)) }
chart_page <- function(draw) { par(oma = c(2, 0, 0, 0)); draw(); mtext(FOOTER, side = 1, outer = TRUE, cex = 0.7, line = 0.5) }

make_filing <- function(path, x) {
  pdf(path, width = 8.5, height = 11, family = "Helvetica")
  on.exit(dev.off())
  text_page("Motor TPL mock rate filing (freMTPL2 public data)", c(
    "Contents", "  Exhibit 1  Overall rate indication and assumptions",
    "  Exhibit 2  Current vs proposed relativities, credibility and caps",
    "  Exhibit 3  Premium dislocation", "  Exhibit 4  Model support", "",
    "Every assumption in Exhibit 1 except experience losses and exposure is illustrative.",
    "The holdout is a random 80/20 split stratified by claim count; the data carry no policy dates,",
    "so this is not a forward in time test.", "", x$summary_lines))
  text_page("Exhibit 1. Overall rate indication (loss ratio method)", c(tbl(x$assumptions), "", x$indication_lines))
  text_page("Exhibit 2. Current vs proposed relativities", c(
    "indicated = GLM frequency relativity x GLM severity relativity (severity = 1 when the severity GLM fails holdout, as here);",
    "Z = min(1, sqrt(claims / 1082));",
    "credibility weighted = Z x indicated + (1 - Z) x current; final = credibility weighted, change capped at +/- 15%.", "",
    tbl(x$rel_exhibit)), cex = 0.55, per = 80)
  text_page("Exhibit 3. Premium dislocation", c(tbl(x$disloc_table), "", x$disloc_lines))
  par(mfrow = c(1, 1)); chart_page(function() plot_disloc(x$disloc))
  text_page("Exhibit 4a. Holdout model comparison (test set)", c(tbl(x$model_table), "", tbl(x$gini_table)))
  par(mfrow = c(2, 1), mar = c(4.5, 4.5, 3, 1)); chart_page(function() { plot_lift(x$lift); plot_double(x$dl_cp, "current", "proposed", "Double lift, current vs proposed pure premium") })
  par(mfrow = c(1, 1)); chart_page(function() plot_oneway(x$rel, x$vars))
  text_page("Exhibit 4b. Proposed frequency GLM coefficients (Poisson, log link, exposure offset)", tbl(x$coef_freq), cex = 0.55, per = 80)
  text_page("Exhibit 4c. Severity GLM coefficients (Gamma, log link, capped average claim)", tbl(x$coef_sev))
}
