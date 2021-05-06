#include <Rcpp.h>
#include "TridiagConst.h"

TridiagConst::TridiagConst()
{
  lower = -0.01;
  diagonal = 0.99;
  upper = -0.01;
  target = {1.0, 1.0, 1.0};
}

TridiagConst::TridiagConst(double a, double b, double c, Rcpp::NumericVector d)
{
  lower = a;
  diagonal = b;
  upper = c;
  target = d;
}

TridiagConst::~TridiagConst()
{

}

void TridiagConst::setTarget(Rcpp::NumericVector d)
{
  target = d;
}

// Thomas Algorithm for constant Tridiagonal systems
Rcpp::NumericVector TridiagConst::trisolve()
{
  // On paper, diagonal and target are from 1 to n, the vector lower, the lower diag is from 2 to n, and upper, the upper diag, is from 1 to n-1.
  int n = target.size();
  // To preserve the coefficients, we make new ones for the forward sweep and x for the solution to be returned.
  Rcpp::NumericVector cc(n - 1);
  Rcpp::NumericVector dd(n);
  Rcpp::NumericVector x(n);
  cc[0] = upper / diagonal;
  // Forward sweep for upper vector
  for (int i = 1; i < n - 1; ++i)
  {
    cc[i] = upper / (diagonal - lower * cc[i - 1]);
  }
  // Repeating nearly the same for target
  dd[0] = target[0] / diagonal;
  for (int i = 1; i < n; ++i)
  {
    // On paper a_i is written indexed from 2 to n, but since the values in the computer are indexed starting from 0, to n-1, and we are looping from 1 to n-1, we must decrement a_i to a_{i-1}
    // It actually doesn't matter for a_i=lower constants
    dd[i] = (target[i] - lower * dd[i - 1]) / (diagonal - lower * cc[i - 1]);
  }
  // Back substitution for the solution
  x[n - 1] = dd[n - 1];
  for (int i = n - 2; i >= 0; --i)
  {
    x[i] = dd[i] - cc[i] * x[i + 1];
  }
  return x;
}
