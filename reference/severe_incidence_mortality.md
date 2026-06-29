# Adjust severe rates as a result of treatment coverage and estimate mortality disaggregated by hospitalised and community

See[Griffin et al
(2016)](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext)
SI for more details

## Usage

``` r
severe_incidence_mortality(
  x,
  treatment_scaler,
  hosp_sev_cfr,
  community_sev_cfr
)
```

## Arguments

- x:

  Input data.frame

- treatment_scaler:

  Proportion of treated severe cases averted by treatment

- hosp_sev_cfr:

  Severe case fatality ratio in hospital The original estimate fitted to
  Reyburn et al data in Griffin et al (2016) 0.065

- community_sev_cfr:

  Severe case fatality ratio in the commnity (non-hospitalised) The
  original estimate fitted to Lubell et al data in Griffin et al (2016)
  0.6
