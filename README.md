
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fragility.index

<!-- badges: start -->

<!-- badges: end -->

The fragility index is gaining widespread adoption as a means of
quantifying the strength of evidence in randomised controlled trials. Dr
Palmer, Dr Marra and Professor Singer use in-silico simulation to
respectfully challenge the use of the fragility index in this context.

## Key Takeaway Points

  - The fragility index is receiving increased attention as a means of
    quantifying the strength of evidence from a randomised controlled
    trial (RCT).
  - The fragility index can however be viewed simply as a transformation
    of the p value adjusted for observed study characteristics
    (e.g. sample size), and thus contains no additional information.
  - The fragility index should not be used to quantify the strength of
    evidence from RCTs

## Installation

You can install the latest version of this research compendium from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("DocEd/fragility.index")
```

## Running the Simulation

You can access the simulation via for following code:

``` r
library(fragility.index)
simulate_fragility(.x, .y, control_mort = 0.3, permit_negative = TRUE)
```

## Bug Reports / Issues

If you find any errors, please submit an issue request. If you would
like to discuss the paper in more detail, please get in touch.
