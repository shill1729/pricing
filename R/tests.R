# # Test the MSE between analytic formulas and Monte-Carlo
# # for a single expiration
# library(pricing)
# strikes <- 100:110
# expiries <- 1
# spot <- 100
# type <- "put"
# param <- c(0, 0.5)
# samples <- 10000
# N <- 300
# M <- 300
# american <- FALSE
# model <- list(name = "gbm", param = param)
# exact <- pricer_analytic(strikes, expiries, spot, model, type)
# mc <- pricer_mc(strikes, expiries, spot, model, type, n = samples)
# pde <- pricer_pde(strikes, expiries, spot, model, type, N, M, american)
# # mse_mc <- mean((exact[, 2]-mc[, 2])^2)
# # pde_mc <- mean((exact[, 2]-pde[, 2])^2)
# # print(head(cbind(strike = exact$strike,
# #                  exact = exact[, 2],
# #                  mc = mc[, 2],
# #                  pde = pde[, 2]
# #                  )
# #            ))
# # print(c(mc_error = mse_mc, pde_error = pde_mc))
