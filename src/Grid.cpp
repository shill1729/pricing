#include <Rcpp.h>
#include "Grid.h"

Grid::Grid()
{
  space_lb = -1.0;
  space_ub = 1.0;
  maturity = 5.0 / 252.0;
  timeRes = 10;
  spaceRes = 10;

}

Grid::Grid(double a, double b, double T, int N, int M)
{
  space_lb = a;
  space_ub = b;
  maturity = T;
  timeRes = N;
  spaceRes = M;
}

Grid::Grid(double B, double T, int N, int M)
{
  space_lb = -B;
  space_ub = B;
  maturity = T;
  timeRes = N;
  spaceRes = M;
}

Grid::~Grid()
{

}

double Grid::getSpaceStepSize()
{
  return (space_ub - space_lb) / spaceRes;
}

double Grid::getTimeStepSize()
{
  return maturity / timeRes;
}

Rcpp::NumericVector Grid::getSpaceGrid()
{
  return discretizeInterval(space_lb, space_ub, spaceRes);
}

Rcpp::NumericVector Grid::getTimeGrid()
{
  return discretizeInterval(0.0, maturity, timeRes);
}

Rcpp::NumericVector Grid::discretizeInterval(double a, double b, int n)
{
  Rcpp::NumericVector x(n + 1);
  double h = (b - a) / n;
  for(int i = 0; i < x.size(); i++)
  {
    x[i] = a + i*h;
  }
  return x;
}
