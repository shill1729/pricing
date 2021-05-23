#include <Rcpp.h>
#include "TridiagConst.h"
#include "Payoff.h"
#include "Grid.h"

// Assuming we are given a grid and payoff with strike and maturity and local space-region
Rcpp::NumericMatrix blackScholesPDE(double spot, Rcpp::NumericVector param, Payoff payoff, Grid grid, bool american)
{
  // Extract risk-neutral rate and volatility
  double r = param[0];
  double v = param[1];
  // Extract step-sizes and resolution
  double h = grid.getSpaceStepSize();
  double k = grid.getTimeStepSize();
  int N = grid.timeRes;
  int M = grid.spaceRes;
  // Get log-price grid and compute the payoff over it
  Rcpp::NumericVector x = grid.getSpaceGrid();
  Rcpp::NumericVector p = payoff.getPayoff(spot, x);
  // Declare solution matrix
  Rcpp::NumericMatrix u(N + 1, M + 1);
  // Initial Conditions: payoff
  for (int j = 0; j < u.ncol(); j++)
  {
    u(0, j) = p[j];
  }
  // Boundary Conditions: assumed ~ payoff
  for (int i = 0; i < u.nrow(); i++)
  {
    u(i, 0) = p[0];
    u(i, M) = p[M];
  }
  // Parameters for tridiagonal system derived from implicit scheme of the PDE
  double a = (r - 0.5 * v * v) / (2 * h);
  double b = (v * v) / (2 * h * h);
  double alpha = b - a;
  double beta = -r - 2 * b;
  double delta = a + b;
  TridiagConst tridiag(-k*alpha, 1-k*beta, -k*delta, {1, 1});
  // Time-stepping integration
  for (int i = 1; i < N + 1; i++)
  {
    // RHS of tridiagonal system
    Rcpp::NumericVector d(M - 1);
    for (int j = 0; j < M - 1; j++)
    {
      d[j] = u(i - 1, j + 1);
    }
    d[0] += k*alpha * u(0, 0);
    d[M - 2] += k*delta * u(0, M);
    // Solving the system and repopulating
    tridiag.setTarget(d);
    Rcpp::NumericVector sol = tridiag.trisolve();
    for(int j = 1; j <= M - 1; j++)
    {
      if(american)
      {
        u(i, j) = std::max<double>(sol[j - 1], p[j - 1]);
      } else
      {
        u(i, j) = sol[j - 1];
      }
    }
  }
  return u;
}


// [[Rcpp::export]]
Rcpp::NumericMatrix blackScholesPDE(double strike, double maturity, double spot, std::string type, Rcpp::NumericVector param, int N, int M, bool american, double B)
{
  Payoff payoff(strike, type);
  Grid grid(B, maturity, N, M);
  Rcpp::NumericMatrix u = blackScholesPDE(spot, param, payoff, grid, american);
  return u;
}


// [[Rcpp::export]]
Rcpp::NumericVector blackScholesPDE_chain(Rcpp::NumericVector strike, double maturity, double spot, std::string type, Rcpp::NumericVector param, int N, int M, bool american)
{

  int n = strike.size();
  Rcpp::NumericVector prices(n);
  double B = 0.5*param[1]*M*std::sqrt(3*maturity/N);
  Grid grid(B, maturity, N, M);
  for(int i = 0; i < n; i++)
  {
    Payoff payoff(strike[i], type);
    Rcpp::NumericMatrix u = blackScholesPDE(spot, param, payoff, grid, american);
    prices[i] = u(N, M / 2);
  }
  return prices;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix blackScholesPDE_surface(Rcpp::NumericVector strike, Rcpp::NumericVector maturity, double spot, std::string type, Rcpp::NumericVector param, int N, int M, bool american)
{

  int m = strike.size();
  int n = maturity.size();
  Rcpp::NumericMatrix prices(m, n);
  for(int i = 0; i < n; i++)
  {
    prices(Rcpp::_, i) = blackScholesPDE_chain(strike, maturity[i], spot, type, param, N, M, american);
  }
  return prices;
}

Rcpp::NumericVector computeGreeks(double strike, double maturity, double spot, Rcpp::NumericMatrix u, int N, int M, bool american, double B)
{

  Rcpp::NumericVector greeks(5);
  double h = 2.0*B/M;
  double k = maturity/N;
  int i = M / 2;
  double Fee = u(N, i);
  double Delta = (u(N, i + 1) - u(N, i - 1)) / (2 * h);
  double Gamma = (u(N, i + 1) - 2 * u(N, i) + u(N, i - 1)) / (h*h);
  double Theta = (u(N - 1, i) - u(N, i)) / k;
  Theta = Theta / 360.0;
  greeks[0] = strike;
  greeks[1] = Fee;
  greeks[2] = Delta / spot;
  greeks[3] = (Gamma - Delta) / (spot*spot);
  greeks[4] = Theta;
  return greeks;
}

// [[Rcpp::export]]
Rcpp::NumericVector blackScholesGreeks_chain(Rcpp::NumericVector strike, double maturity, double spot, std::string type, Rcpp::NumericVector param, int N, int M, bool american)
{

  int n = strike.size();
  Rcpp::NumericMatrix greeks(n, 5);
  double B = 0.5*param[1]*M*std::sqrt(3*maturity/N);
  Grid grid(B, maturity, N, M);
  for(int i = 0; i < n; i++)
  {
    Payoff payoff(strike[i], type);
    Rcpp::NumericMatrix u = blackScholesPDE(spot, param, payoff, grid, american);
    Rcpp::NumericVector v = computeGreeks(strike[i], maturity, spot, u, N, M, american, B);
    greeks(i, Rcpp::_) = v;
  }
  return greeks;
}

// [[Rcpp::export]]
Rcpp::NumericVector blackScholesGreeks(double strike, double maturity, double spot, std::string type, Rcpp::NumericVector param, int N, int M, bool american)
{
  Rcpp::NumericVector greeks(5);
  double B = 0.5*param[1]*M*std::sqrt(3*maturity/N);
  Grid grid(B, maturity, N, M);
  Payoff payoff(strike, type);
  Rcpp::NumericMatrix u = blackScholesPDE(spot, param, payoff, grid, american);
  greeks = computeGreeks(strike, maturity, spot, u, N, M, american, B);
  return greeks;
}

