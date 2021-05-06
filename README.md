
# pricing

<!-- badges: start -->
<!-- badges: end -->

This package provides option pricing functions for the following models defining price dynamics:

- [x] Black-Scholes
- [x] Merton's jump diffusion 
- [x] Log-normal mixture diffusion
- [x] geometric compensated Poisson

For European options, a Monte-Carlo routine is available for every model of price dynamics, and (semi-)analytic formulas are available for the Black-Scholes and Merton models.

American options cannot be priced with a straight forward Monte-Carlo integrator as they are path-dependent (they are defined as the supremum over all stopping times up to the maturity of the discounted expected value, thus depend on the price path up to this time, unlike European options which only depend on the terminal value, and hence its distribution). So, we also provide PDE/PIDE methods for pricing European and American options via finite-difference solvers. The FD engines currently only use the implicit-scheme, and for the PIDE solver, an implicit-explicit scheme adjoining a trapezoid integral approximation for the jump-integral of the PIDE.

## Installation

You can install the latest GitHub version via the devtools package

``` r
devtools::install_github("shill1729/pricing")
```

## Example
TODO
``` r
library(pricing)
```

