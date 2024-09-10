
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ranon

<!-- badges: start -->

[![R-CMD-check](https://github.com/osaal/ranon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/osaal/ranon/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`ranon` is a package for inspecting human data sets for anonymity. It is
in early development.

Currently, the package supports the following anonymity checks:

- **k-anonymity** (function `k-anonymity()`)
- **categorical variable cell counts** (function `check_count()`)

## Installation

You can install the development version of ranon from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("osaal/ranon")
```

The development package uses `renv` for environment management. Install
the necessary development packages locally with:

``` r
# install.packages("renv")
renv::restore() 
```

## Examples

Examples will be added later.
