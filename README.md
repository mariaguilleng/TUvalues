# TUvalues: Tools for Calculating Allocations in Game Theory using Exact and Approximated Methods

The main objective of cooperative Transferable-Utility games (TU-games) is to allocate a good among the agents involved. The package implements major solution concepts including the Shapley value, Banzhaf value, and egalitarian rules, alongside their extensions for structured games: the Owen value and Banzhaf-Owen value for games with a priori unions, and the Myerson value for communication games on networks. To address the inherent exponential computational complexity of exact evaluation, the package offers both exact algorithms and linear approximation methods based on sampling, enabling the analysis of large-scale games. Additionally, it supports core set-based solutions, allowing computation of the vertices and the centroid of the core.

## Installation

You can install the released version of the package from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("TUvalues")
```

And the development version from
[GitHub](https://github.com/mariaguilleng/TUvalues) with:

``` r
devtools::install_github("mariaguilleng/TUvalues")
```
