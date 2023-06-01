
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postie <img src="man/figures/postie_hex.png" align="right" width=30% height=30% />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/postie/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/postie/actions)
[![Coverage
status](https://codecov.io/gh/mrc-ide/postie/branch/main/graph/badge.svg)](https://codecov.io/github/mrc-ide/postie)
<!-- badges: end -->

Use postie to post-process
[malariasimulation](https://mrc-ide.github.io/malariasimulation/) model
output.

Postie requires some conventions in the raw model outputs to work:

1.  Age-bands for clinical and severe incidence must be the same, and
2.  The number of individuals in each age group must be output, also
    with the same age bands.

In practice this means you will need to set:

    parameters$clinical_incidence_rendering_min_ages = min_ages
    parameters$clinical_incidence_rendering_max_ages = max_ages
    parameters$severe_incidence_rendering_min_ages = min_ages
    parameters$severe_incidence_rendering_max_ages = max_ages
    parameters$age_group_rendering_min_ages = min_ages
    parameters$age_group_rendering_max_ages = max_ages