# # Test the MSE between analytic formulas and Monte-Carlo
# # for a single expiration
# library(pricing)
# strikes <- 100:110
# expiries <- 5/252
# spot <- 100
# type <- "call"
# param <- c(0, 0.5, 10, 0, 0.07)
# samples <- 10000
# model <- list(name = "merton", param = param)
# w <- pricer_analytic(strikes, expiries, spot, model, type)
# ww <- pricer_mc(strikes, expiries, spot, model, type, n = samples)
# mse <- mean((w[, 2]-ww[, 2])^2)
# print(head(cbind(strike = w$strike, exact = w[, 2], mc = ww[, 2])))
# print(mse)
