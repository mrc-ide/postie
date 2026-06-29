# Extract basic rates from model output

Extract basic rates from model output

## Usage

``` r
get_prevalence(
  x,
  diagnostic = "lm",
  baseline_year = 2000,
  ages_as_years = TRUE
)
```

## Arguments

- x:

  Input data.frame

- diagnostic:

  Can be light microscopy ("lm"), or PCR ("pcr")

- baseline_year:

  Baseline year (assumes simulation starts on the first day of the year)

- ages_as_years:

  Convert ages to be in units of years
