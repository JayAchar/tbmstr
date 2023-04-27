# tbmstr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

A R package containing code to analyse data from the European modified
short treatment regimen for drug resistant tuberculosis study. 

The package includes example data and other materials to support an advanced
data anlaysis course for study implementers.

## Installation

This package can be installed by running the following code in your R
environment: 

```r
devtools::install_github("JayAchar/tbmstr")
```

## Load tutorials

This package includes a number of `learnr` interactive tutorials. To access
them, run the following code with the tutorial name:

```r
learnr::run_tutorial("tutorial_name", "tbmstr")
```

## Development

### Unit testing

```r
testthat::auto_test_package()
```
