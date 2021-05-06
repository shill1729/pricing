#ifndef TRIDIAG_H
#define TRIDIAG_H

class Tridiag {
public:
  Tridiag();
  Tridiag(Rcpp::NumericVector a, Rcpp::NumericVector b, Rcpp::NumericVector c, Rcpp::NumericVector d);
  virtual ~Tridiag();
  // Member data: linear system Ax=target, where A is a tridiagonal matrix.
  Rcpp::NumericVector lower;
  Rcpp::NumericVector diagonal;
  Rcpp::NumericVector upper;
  Rcpp::NumericVector target;
  // Methods
  Rcpp::NumericVector trisolve();
};

#endif
