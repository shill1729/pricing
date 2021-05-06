#' A generic Monte-Carlo integrator
#'
#' @param maturity the maturity of the option
#' @param rate the discounting rate
#' @param variates the simulated terminal payoff variates to average
#'
#' @description {A generic Monte-Carlo integrator given a vector of
#' simulated terminal payoff variates.}
#' @return numeric
monteCarlo <- function(maturity, rate, variates)
{
  exp(-rate*maturity)*mean(variates)
}

#' Monte-Carlo European option pricing
#'
#' @param strikes vector of strike prices
#' @param expiries vector of maturities, in trading years
#' @param spot the current spot price of the underlying
#' @param model the dynamics defining the model, see details
#' @param type the type of option to price
#' @param n number of variates in Monte-Carlo integrator
#'
#' @description {Compute European option prices under three basic models: Black-Scholes,
#' Merton's jump diffusion, and a log-normal mixture.}
#' @details {The argument \code{model} must be a named list of
#' \itemize{
#' \item \code{name} either "gbm", "mixture", or "merton"
#' \item \code{param} the parameters defining the above model.}
#' For "gbm" and "merton", \code{param} should be a vector of the risk-free rate,
#' volatility, and the same with the mean rate of jumps and jump parameters. For
#' "mixture" it must be a matrix of probabilities, risk-neutral rate, and volatilities. For
#' "gcpp", it should be a vector of the risk-free rate, jump rate, jump size, and compensator size.}
#' @return data.frame
#' @export pricer_mc
pricer_mc <- function(strikes, expiries, spot, model, type = "call", n = 1000)
{

  rate <- 0
  x <- 0
  s <- 0
  if(model$name != "mixture")
  {
    rate <- model$param[1]
  } else
  {
    rate <- model$param[2, 1]
  }

  prices <- matrix(0, nrow = length(strikes), ncol = length(expiries))
  for(j in 1:length(expiries))
  {
    variates <- matrix(0, nrow = length(strikes), ncol = n)
    # Should we generate one set of n variates for every-strike?
    ####: Faster but perhaps inaccurate?
    if(model$name == "merton")
    {
      # rmerton returns the log-variates
      x <- findistr::rmerton(n, expiries[j], model$param)
      s <- spot*exp(x)
    } else if(model$name == "gbm")
    {
      s <- findistr::rgbm(n, expiries[j], spot, rate, model$param[2])
    } else if(model$name == "mixture")
    {
      # rgmm returns the log-variates
      x <- findistr::rgmm(n, model$param[1, ], (rate-0.5*model$param[3, ]^2)*expiries[j], model$param[3, ]*sqrt(expiries[j]))
      s <- spot*exp(x)
    } else if(model$name == "gcpp")
    {
      s <- findistr::rgcpp(n, expiries[j], spot, model$param[3], model$param[4], model$param[2])
    }
    ####
    for(i in 1:length(strikes))
    {
      # Or place it here and generate new variates for each strike?
      # Slower but perhaps more inaccurate?
      if(type == "put")
      {
        variates[i, ] <- putOption(s, strikes[i])
      } else if(type == "call")
      {
        variates[i, ] <- callOption(s, strikes[i])
      }
      prices[i, j] <- monteCarlo(expiries[j], rate, variates[i, ])
    }
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
