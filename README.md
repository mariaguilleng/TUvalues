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

## Reproducing the results of the paper

The repository also contains the code of the experiments in the paper:

> Camacho, J., Gonçalves-Dosantos, J. C., & Guillen, M. D. TUvalues: Exact
> and Approximate Allocation Rules for Transferable-Utility Games in R.
> *SoftwareX* (under review).

These folders are not part of the CRAN package:

- `examples/`: the usage examples of Section 3.3, with exact
  (`exact_example.R`) and approximate (`approximate_example.R`) calculations.
- `benchmarks/`: scalability and approximation-accuracy experiments
  (Section 3.2). `run_benchmarks.R` runs all the games and builds the figures
  and tables.
- `package_comparison/`: comparison with TUGLab, CoopGame and GameTheory
  (Section 3.1).

Run the scripts from the repository root, for example:

``` sh
Rscript benchmarks/run_benchmarks.R
```

Besides TUvalues, the experiments use ggplot2, readxl, writexl, callr, geometry,
microbenchmark, TUGLab, CoopGame and GameTheory.
