#include <Rcpp.h>
#include "Tridiag.h"

Tridiag::Tridiag()
{
  lower = {0, 0};
  diagonal = {0.9, 0.9, 0.9};
  upper = {0, 0};
  target = {1, 1, 1};
}

Tridiag::Tridiag(Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector c, Rcpp::NumericVector d)
{
  lower = a;
  diagonal = b;
  upper = c;
  target = d;
}

Tridiag::~Tridiag()
{

}


// Thomas Algorithm for tridiagonal systems
Rcpp::NumericVector Tridiag::trisolve()
{
  // On paper, diagonal and target are from 1 to n, the vector lower, the lower diag is from 2 to n, and upper, the upper diag, is from 1 to n-1.
  int n = target.size();
  // To preserve the coefficients, we make new ones for the forward sweep and x for the solution to be returned.
  Rcpp::NumericVector cc(n - 1);
  Rcpp::NumericVector dd(n);
  Rcpp::NumericVector x(n);
  cc[0] = upper[0] / diagonal[0];
  // Forward sweep for upper vector
  for (int i = 1; i < n - 1; ++i)
  {
    cc[i] = upper[i] / (diagonal[i] - lower[i - 1] * cc[i - 1]);
  }
  // Repeating nearly the same for target
  dd[0] = target[0] / diagonal[0];
  for (int i = 1; i < n; ++i)
  {
    // On paper a_i is written indexed from 2 to n, but since the values in the computer are indexed starting from 0, to n-1, and we are looping from 1 to n-1, we must decrement a_i to a_{i-1}
    // It actually doesn't matter for a_i=lower constants
    dd[i] = (target[i] - lower[i - 1] * dd[i - 1]) / (diagonal[i] - lower[i - 1] * cc[i - 1]);
  }
  // Back substitution for the solution
  x[n - 1] = dd[n - 1];
  for (int i = n - 2; i >= 0; --i)
  {
    x[i] = dd[i] - cc[i] * x[i + 1];
  }
  return x;
}
