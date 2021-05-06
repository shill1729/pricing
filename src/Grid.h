#ifndef GRID_H
#define GRID_H

class Grid {
public:
  Grid();
  // Specify a grid with space LB/UB and time-length + resolutions
  Grid(double a, double b, double T, int N, int M);
  // Specify a grid with a symmetric space interval [-B, B], etc.
  Grid(double B, double T, int N, int M);
  virtual ~Grid();
  // Member data
  double space_lb;
  double space_ub;
  double maturity;
  int timeRes;
  int spaceRes;
  // Member methods
  double getSpaceStepSize();
  double getTimeStepSize();
  Rcpp::NumericVector getSpaceGrid();
  Rcpp::NumericVector getTimeGrid();

private:
  Rcpp::NumericVector discretizeInterval(double a, double b, int n);
};

#endif
