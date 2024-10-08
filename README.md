
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postie <img src="man/figures/Postie.png" align="right" width=30% height=30% />

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

Postie requires are few conventions to work:

1.  that age-bands for clinical and severe incidence must be the same.
    In practice this means you will need to set, for example:

<!-- -->

    year <- 365
    min_ages <- year * 0:99
    max_ages <- year * 1:100
    parameters$clinical_incidence_rendering_min_ages = min_ages
    parameters$clinical_incidence_rendering_max_ages = max_ages
    parameters$severe_incidence_rendering_min_ages = min_ages
    parameters$severe_incidence_rendering_max_ages = max_ages

2.  that `ft` (treatment coverage) is an output variable. This will most
    likely be a result of setting treatment in the simulation, for
    example:

<!-- -->

    parameters |>
    malariasimulation::set_drugs(list(malariasimulation::AL_params)) |>
    malariasimulation::set_clinical_treatment(1, 50, 0.5)

alternatively, if no treatment is implemented, you could just add
`ft = 0` to the simulation output before using postie.
