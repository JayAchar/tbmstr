# tbmstr

<!-- badges: start -->

[![R-CMD-check](https://github.com/JayAchar/tbmstr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JayAchar/tbmstr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

A R package containing code to analyse data from the European modified
short treatment regimen for drug resistant tuberculosis study.

The package includes example data and other materials to support an advanced
data analysis course for study implementers. In addition, the analysis for a
peer-reviewed publication is stored in a
[`targets`](https://books.ropensci.org/targets/) workflow along with a data
sharing workflow.

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

## Workflows

### Peer-reviewed analysis

The analysis is stored in the `inst/analysis` directory as a `targets` workflow.
To run the workflow, source the script - `inst/analysis/run_manuscript.R`. This
will set an environment variable and run the cached analysis. The results
will be returned in the `inst/analysis/output` directory.

### Data sharing

A data sharing workflow has also been defined in the `inst/data-sharing`
directory. This takes the raw study data and returns data sets for the regional
study and each country along with a data dictionary.

Source the run script to execute the workflow: `inst/data-sharing/_sharing.R`

## Development

### Make

This package includes a `Makefile` to simplify the testing, building and
package installation process.

### Analyses

Scripts have been created to draft analyses. To speed up development,
run the following command in the command line:

```sh
watchexec -e Rmd,R "Rscript -e \"targets::tar_make(script = '.inst/analyses/_manuscript.R')\""
```
