#include <Rcpp.h>
#include "Payoff.h"

Payoff::Payoff()
{
  strike = 100;
}

Payoff::Payoff(double K, std::string type)
{
  strike = K;
  payoffType = type;
}

Payoff::~Payoff()
{

}

void Payoff::setStrike(double K)
{
  strike = K;
}

void Payoff::setPayoffType(std::string type)
{
  payoffType = type;
}

Rcpp::NumericVector Payoff::getPayoff(double spot, Rcpp::NumericVector x)
{
  Rcpp::NumericVector g(x.size());
  if(payoffType == "call")
  {
    for(int i = 0; i < g.size(); i++)
    {
      g[i] = callOption(spot*std::exp(x[i]));
    }
  } else if(payoffType == "put")
  {
    for(int i = 0; i < g.size(); i++)
    {
      g[i] = putOption(spot*std::exp(x[i]));
    }
  }
  return g;
}

double Payoff::callOption(double s)
{
  return std::max<double>(s-strike, 0.0);
}

double Payoff::putOption(double s)
{
  return std::max<double>(strike-s, 0.0);
}
