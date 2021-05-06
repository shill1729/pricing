
#' Analytic European option pricing under three basic models
#'
#' @param strike a single strike price
#' @param expiry a single expiry/maturity in YTE
#' @param spot the current spot price of the underlying
#' @param model the dynamics defining the model, see details
#' @param type the type of option to price
#'
#' @description {Compute European option prices under three basic models: Black-Scholes,
#' Merton's jump diffusion, and a log-normal mixture.}
#' @details {The argument \code{model} must be a named list of
#' \itemize{
#' \item \code{name} either "gbm", "gcpp", "mixture", or "merton"
#' \item \code{param} the parameters defining the above model.}
#' For "gbm" and "merton", \code{param} should be a vector of the risk-free rate,
#' volatility, and the same with the mean rate of jumps and jump parameters. For
#' "mixture" it must be a matrix of probabilities, risk-neutral rate, and volatilities. For
#' "gcpp", it should be a vector of the risk-free rate, jump rate, jump size, and compensator size.}
#' @return numeric
analyticPricer <- function(strike, expiry, spot, model, type = "call")
{
  rate <- 0
  price <- 0
  if(model$name != "mixture")
  {
    rate <- model$param[1]
  } else
  {
    rate <- model$param[2, 1]
  }
  if(model$name == "gbm")
  {
    volat <- model$param[2]
    rn <- findistr::pgbm(strike, expiry, spot, rate, volat)
    fr <- findistr::pgbm(strike, expiry, spot, rate+volat^2, volat)
    if(type == "put")
    {
      price <- strike*exp(-rate*expiry)*rn-spot*fr

    } else if(type == "call")
    {
      price <- spot*(1-fr)-strike*exp(-rate*expiry)*(1-rn)
    }

  } else if(model$name == "mixture")
  {
    stop("'mixture' pricing not implemented analytically yet")

  } else if(model$name == "merton")
  {
    # Extract jump-diffusion parameters
    volat <- model$param[2]
    lambda <- model$param[3]
    jm <- model$param[4]
    jv <- model$param[5]
    # Compute mean-jump size
    eta <- exp(jm+jv^2/2)-1
    # adjusted jump rate
    lam <- lambda*(1+eta)
    num_terms <- stats::qpois(0.99, lambda = lam*expiry)
    n  <- 0:num_terms
    pp <- stats::dpois(n, lambda = lam*expiry)
    # Adjusted rates and volatility
    sigma_n <- sqrt(volat^2+(n*jv^2)/expiry)
    rtilde <- rate-lambda*eta-0.5*volat^2+n*jm/expiry
    r_n <- rtilde+sigma_n^2/2
    condPrices <- matrix(0, nrow = n+1)
    for(i in 1:(num_terms+1))
    {
      param <- c(r_n[i], sigma_n[i])
      # Recursively compute Black-Scholes prices conditional on n jumps
      condModel <- list(name = "gbm", param = param)
      condPrices[i] <- analyticPricer(strike, expiry, spot, condModel, type)
    }
    price <- sum(condPrices*pp)
  } else if(model$name == "gcpp")
  {
    rate <- model$param[1]
    lambda <- model$param[2]
    a <- model$param[3]
    b <- model$param[4]
    lambdaStar <- (rate+b)/(exp(a)-1)
    rn <- findistr::pgcpp(strike, expiry, spot, a, b, lambdaStar)
    fr <- findistr::pgcpp(strike, expiry, spot, a, b, exp(a)*lambdaStar)
    if(type == "put")
    {

      price <- strike*exp(-rate*expiry)*rn-spot*fr

    } else if(type == "call")
    {
      price <- spot*(1-fr)-strike*exp(-rate*expiry)*(1-rn)
    }
  }
  return(price)
}

#' Analytic European option pricing under three basic models
#'
#' @param strikes vector of strike prices
#' @param expiries vector of maturities, in trading years
#' @param spot the current spot price of the underlying
#' @param model the dynamics defining the model, see details
#' @param type the type of option to price
#'
#' @description {Compute European option prices under three basic models: Black-Scholes,
#' Merton's jump diffusion, and a log-normal mixture.}
#' @details {The argument \code{model} must be a named list of
#' \itemize{
#' \item \code{name} either "gbm", "mixture", or "merton"
#' \item \code{param} the parameters defining the above model.}
#' For "gbm" and "merton", \code{param} should be a vector of the risk-free rate,
#' volatility, and the same with the mean rate of jumps and jump parameters. For
#' "mixture" it must be a matrix of probabilities, risk-neutral rate, and volatilities.}
#' @return data.frame
#' @export pricer_analytic
pricer_analytic <- function(strikes, expiries, spot, model, type = "call")
{
  prices <- matrix(0, nrow = length(strikes), ncol = length(expiries))
  for(j in 1:length(expiries))
  {
    for(i in 1:length(strikes))
    {
      prices[i, j] <- analyticPricer(strikes[i], expiries[j], spot, model, type)
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
