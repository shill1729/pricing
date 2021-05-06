
#' Thomas-algorithm for tridiagonal linear systems
#'
#' @param alpha lower diagonal
#' @param beta diagonal
#' @param delta upper diagonal
#' @param b.vec target vector
#'
#' @description {Solve tridiagonal linear systems with the Thomas-algorithm.}
#' @return numeric vector
trisolveR <- function(alpha, beta, delta, b.vec)
{
  n <- length(b.vec)
  cc <- matrix(0, nrow = n-1)
  dd <- matrix(0, nrow = n)
  x <- matrix(0, nrow = n)
  cc[1] <- delta/beta
  # Forward sweep for delta
  for(i in 2:(n-1))
  {
    cc[i] <- delta/(beta-alpha*cc[i-1])
  }

  # Repeating the nearly same for b.vec
  dd[1] <- b.vec[1]/beta
  for(i in 2:(n))
  {
    # alpha is given vector length of M-2, n = M-1. so i-1 ranges from 1 to n-1= 1 to M-2. looks good.
    dd[i] <- (b.vec[i]-alpha*dd[i-1])/(beta-alpha*cc[i-1])
  }

  x[n] <- dd[n]
  for(i in (n-1):1)
  {
    x[i] <- dd[i]-cc[i]*x[i+1]
  }
  return(x)
}
