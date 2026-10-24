source(file.path("..", "..", "R", "rating.R"))

test_that("base level relativity is exactly 1", {
  set.seed(1)
  d <- data.table(x = factor(sample(c("a", "b", "c"), 3000, TRUE)), Exposure = runif(3000, 0.2, 1))
  d[, y := rpois(.N, Exposure * c(a = 0.1, b = 0.2, c = 0.05)[as.character(x)])]
  d <- set_base(d, base_levels(d, "x"))
  fit <- glm(y ~ x, family = poisson, offset = log(Exposure), data = d)
  r <- relativities(fit, "x", d, levels(d$x)[1])
  expect_equal(unname(r[levels(d$x)[1]]), 1)
  expect_equal(unname(r["b"] / r["a"]), unname(exp(coef(fit)["xb"] - ifelse("xa" %in% names(coef(fit)), coef(fit)["xa"], 0))))
})

test_that("lift bins sum to total exposure", {
  set.seed(2)
  expo <- runif(500, 0.1, 1); pred <- expo * runif(500); actual <- rpois(500, pred)
  lt <- lift_table(pred, actual, expo, n = 10)
  expect_equal(sum(lt$expo), sum(expo))
  expect_equal(nrow(lt), 10)
  dl <- double_lift(pred, pred * runif(500, 0.8, 1.2), actual, expo)
  expect_equal(sum(dl$expo), sum(expo))
})

test_that("indication matches a hand computed case", {
  # loss 1000, LDF 1, trend 3% for 2 years, ULAE 10%, premium 1500, V 25%, Q 5%
  # projected LR = 1000 * 1.0609 * 1.1 / 1500 = 0.777993; permissible = 0.70; indication = 0.111419
  expect_equal(indication(1000, 0.10, 0.03, 2, 1, 1500, 0.25, 0.05), 1000 * 1.0609 * 1.1 / 1500 / 0.7 - 1)
  expect_equal(round(indication(1000, 0.10, 0.03, 2, 1, 1500, 0.25, 0.05), 6), 0.111419)
})

test_that("credibility and capping behave", {
  expect_equal(credibility_z(c(0, 1082, 4328)), c(0, 1, 1))
  expect_equal(credibility_z(270.5), 0.5)
  cc <- cap_change(c(1.3, 1.0, 0.7), c(1, 1, 1))
  expect_equal(cc$final, c(1.15, 1.0, 0.85)); expect_equal(cc$capped, c(TRUE, FALSE, TRUE))
})

test_that("gini is 0 for a constant predictor and positive for a perfect ranking", {
  expo <- rep(1, 100); actual <- c(rep(0, 90), rep(5, 10))
  expect_equal(gini(rep(1, 100), actual, expo), 0, tolerance = 0.02)
  expect_gt(gini(actual, actual, expo), 0.8)
})
