#' The terminal payoff of a put option
#'
#' @param spot the current spot price
#' @param strike the strike price of the option
#'
#' @return numeric or vector
putOption <- function(spot, strike)
{
  return(pmax(strike-spot, 0))
}

#' The terminal payoff of a call option
#'
#' @param spot the current spot price
#' @param strike the strike price of the option
#'
#' @return numeric or vector
callOption <- function(spot, strike)
{
  return(pmax(spot-strike, 0))
}

#' The terminal payoff of a put debit spread
#'
#' @param spot the current spot price
#' @param strikes a vector of two strikes in ascending order
#'
#' @return numeric or vector
putDebitSpread <- function(spot, strikes)
{
  if(!is.vector(strikes))
  {
    stop("argument 'strikes' must be a vector of length 2")
  } else
  {
    if(length(strikes) != 2)
    {
      stop("argument 'strikes' must be a vector of length 2")
    } else
    {
      if(strikes[1] >= strikes[2])
      {
        stop("the vector 'strikes' must be in ascending order")
      }
    }
  }
  # Strikes must be ascending:
  payoff <- putOption(spot, strikes[2])-putOption(spot, strikes[1])
  return(payoff)
}

#' The terminal payoff of a call debit spread
#'
#' @param spot the current spot price
#' @param strikes a vector of two strikes in ascending order
#'
#' @return numeric or vector
callDebitSpread <- function(spot, strikes)
{
  if(!is.vector(strikes))
  {
    stop("argument 'strikes' must be a vector of length 2")
  } else
  {
    if(length(strikes) != 2)
    {
      stop("argument 'strikes' must be a vector of length 2")
    } else
    {
      if(strikes[1] >= strikes[2])
      {
        stop("the vector 'strikes' must be in ascending order")
      }
    }
  }
  # Strikes must be ascending:
  payoff <- callOption(spot, strikes[1])-callOption(spot, strikes[2])
  return(payoff)
}

