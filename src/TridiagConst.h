#ifndef TRIDIAGCONST_H
#define TRIDIAGCONST_H

class TridiagConst {
public:
  TridiagConst();
  TridiagConst(double a, double b, double c, Rcpp::NumericVector d);
  virtual ~TridiagConst();
  // Member data
  double lower;
  double diagonal;
  double upper;
  Rcpp::NumericVector target;
  // Methods
  void setTarget(Rcpp::NumericVector d);
  Rcpp::NumericVector trisolve();
};

#endif
