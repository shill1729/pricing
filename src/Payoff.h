#ifndef PAYOFF_H
#define PAYOFF_H

class Payoff {
public:
  Payoff();
  Payoff(double K, std::string type);
  virtual ~Payoff();
  // Member data
  double strike;
  std::string payoffType;
  // Member methods
  void setStrike(double K);
  void setPayoffType(std::string type);
  // Compute payoff over a log-price grid
  Rcpp::NumericVector getPayoff(double spot, Rcpp::NumericVector x);
private:
  double putOption(double s);
  double callOption(double s);
};
#endif
