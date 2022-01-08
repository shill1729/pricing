#' Signed IV error
#'
#' @param v the volatility guess
#' @param strike the strike price of the option to backout parameters for
#' @param maturity the maturity of the option, in trading years
#' @param spot the spot price of the underlying
#' @param price the market price of the option contract
#' @param rate the risk-free rate
#' @param type the type of option: put or call
#' @param N number of time sub-intervals
#' @param M number of space sub-intervals
#'
#' @return numeric
iv_error <- function(v, strike, maturity, spot, price, rate, type, N, M)
{
  model <- list(name = "gbm", param = c(rate, v))
  z <- pricer_pde(strike, maturity, spot, model, type, N, M, TRUE)[1,2]-price
  return(z)
}

#' Signed IR error
#'
#' @param r the risk-free rate guess
#' @param strike the strike price of the option to backout parameters for
#' @param maturity the maturity of the option, in trading years
#' @param spot the spot price of the underlying
#' @param price the market price of the option contract
#' @param volat the volatility level (annualized)
#' @param type the type of option: put or call
#' @param N number of time sub-intervals
#' @param M number of space sub-intervals
#'
#' @return numeric
ir_error <- function(r, strike, maturity, spot, price, volat, type, N, M)
{
  model <- list(name = "gbm", param = c(r, volat))
  z <- pricer_pde(strike, maturity, spot, model, type, N, M, TRUE)[1,2]-price
  return(z)
}


#' Back out the implied volatility of an option given its market price
#'
#' @param strike the strike price of the option to backout parameters for
#' @param maturity the maturity of the option, in trading years
#' @param spot the spot price of the underlying
#' @param price the market price of the option contract
#' @param rate the risk-free rate
#' @param type the type of option: put or call
#' @param N number of time sub-intervals
#' @param M number of space sub-intervals
#' @param ub the upper bound on the volatility search-space
#'
#' @return numeric
#' @export impliedVolatility
impliedVolatility <- function(strike, maturity, spot, price, rate, type, N, M, ub = 5)
{
  stats::uniroot(iv_error, interval = c(0.001, ub), strike = strike,
          maturity = maturity, spot = spot, price = price, rate = rate,
          type = type, N = N, M = M)$root
}

#' Back out the implied volatility of an option given its market price
#'
#' @param strike the strike price of the option to backout parameters for
#' @param maturity the maturity of the option, in trading years
#' @param spot the spot price of the underlying
#' @param price the market price of the option contract
#' @param volat the risk-free rate
#' @param type the type of option: put or call
#' @param N number of time sub-intervals
#' @param M number of space sub-intervals
#' @param ub the bound on the risk-free rate search-space
#'
#' @return numeric
#' @export impliedRate
impliedRate <- function(strike, maturity, spot, price, volat, type, N, M, ub = 5)
{
  stats::uniroot(ir_error, interval = c(-ub, ub), strike = strike,
          maturity = maturity, spot = spot, price = price, volat = volat,
          type = type, N = N, M = M)$root
}
