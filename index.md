# postie ![](reference/figures/Postie.png)

Use postie to post-process
[malariasimulation](https://mrc-ide.github.io/malariasimulation/) model
output.

Postie requires are few conventions to work:

1.  that age-bands for clinical and severe incidence must be the same.
    In practice this means you will need to set, for example:

``` R
year <- 365
min_ages <- year * 0:99
max_ages <- year * 1:100
parameters$clinical_incidence_rendering_min_ages = min_ages
parameters$clinical_incidence_rendering_max_ages = max_ages
parameters$severe_incidence_rendering_min_ages = min_ages
parameters$severe_incidence_rendering_max_ages = max_ages
```

2.  that `ft` (treatment coverage) is an output variable. This will most
    likely be a result of setting treatment in the simulation, for
    example:

``` R
parameters |>
malariasimulation::set_drugs(list(malariasimulation::AL_params)) |>
malariasimulation::set_clinical_treatment(1, 50, 0.5)
```

alternatively, if no treatment is implemented, you could just add
`ft = 0` to the simulation output before using postie.
