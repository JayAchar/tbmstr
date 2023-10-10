# tbmstr

<!-- badges: start -->
[![R-CMD-check](https://github.com/JayAchar/tbmstr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JayAchar/tbmstr/actions/workflows/R-CMD-check.yaml)
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
remotes::install_github("JayAchar/tbmstr")
```

## Load tutorials

This package includes a number of `learnr` interactive tutorials. To access
them, run the following code with the tutorial name:

```r
tbmstr::run_tutorial("tutorial_name")
```

## Analyses development

Scripts have been created to draft analyses. To speed up development, 
run the following command in the command line: 

```sh 
watchexec -e Rmd,R "Rscript -e \"targets::tar_make(script = '.inst/analyses/_targets.R')\""
```
