#' Option pricing via PDE finite-difference solvers
#'
#' @param strikes vector of strike prices
#' @param expiries vector of maturities, in trading years
#' @param spot the current spot price of the underlying
#' @param model the dynamics defining the model, see details
#' @param type the type of option to price
#' @param N time-resolution
#' @param M space-resolution
#' @param american boolean for American options (TRUE) or European options
#'
#' @description {Compute European and American option prices under three basic models: Black-Scholes, and a log-normal mixture. Use
#' the PIDE solver to price under Merton's jump-diffusion.}
#' @details {The argument \code{model} must be a named list of
#' \itemize{
#' \item \code{name} either "gbm" or "mixture"
#' \item \code{param} the parameters defining the above model.}
#' For "gbm", \code{param} should be a vector of the risk-free rate,
#' volatility, and the same with the mean rate of jumps and jump parameters. For
#' "mixture" it must be a matrix of probabilities, risk-neutral rate, and volatilities.}
#' @return data.frame
#' @export pricer_pde
pricer_pde <- function(strikes, expiries, spot, model, type = "call", N = 100, M = 100, american = TRUE)
{
  if(model$name == "gbm")
  {
    prices <- blackScholesPDE_surface(strikes, expiries, spot, type, model$param, N, M, american)
  } else
  {
    stop("Other models not implemented yet for the PDE solver")
  }

  if(!bizdays::has.calendars("trading"))
  {
    trader::date_yte(Sys.Date()+5)
  }
  prices <- cbind(strikes, prices)
  colnames(prices) <- c("strike", as.character(bizdays::offset(Sys.Date(), expiries*252, cal = "trading")))
  prices <- as.data.frame(prices)
  return(prices)
}
