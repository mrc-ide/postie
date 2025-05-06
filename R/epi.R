#' Adjust severe rates as a result of treatment coverage, dis-aggregated by hospitalised and community and estimate mortality
#'
#' See\href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)} SI for
#' more details
#'
#' @param x Input data.frame
#' @param treatment_scaler Proportion of treated severe cases averted by treatment
#' @param hosp_sev_cfr Severe case fatality ratio in hospital
#' The original estimate fitted to Reyburn et al data in Griffin et al (2016) 0.065
#' @param community_sev_cfr Severe case fatality ratio in the commnity (non-hospitalised)
#' The original estimate fitted to Lubell et al data in Griffin et al (2016) 0.6
severe_incidence_mortality <- function(x, treatment_scaler, hosp_sev_cfr, community_sev_cfr){
  if(treatment_scaler > 1 | treatment_scaler < 0){
    stop("treatment_scaler must be between 0 and 1")
  }

  x <- x |>
    # Estimate hospitalised and severe incidence
    dplyr::mutate(
      severe_hospital = .data$severe,
      # Back‑out the non‑hospitalised severe incidence
      severe_community  = (1 - .data$ft_sev) / .data$ft_sev * .data$severe_hospital
    ) |>
    # Rescale for impact of first-line treatment (Assuming no difference in access by severity group)
    dplyr::mutate(
      scaling = (1 - treatment_scaler) * .data$ft,
      severe_hospital = severe_hospital * scaling,
      severe_community = severe_community * scaling,
      severe = severe_hospital + severe_community,
    ) |>
    # Apply location-specific severe case fatality ratios to estimate mortality
    dplyr::mutate(
      mortality_hospital = severe_hospital * hosp_sev_cfr,
      mortality_community = severe_community * community_sev_cfr,
      mortality = mortality_hospital + mortality_community
    ) |>
    dplyr::select(-"scaling") |>
    dplyr::select(-"ft") |>
    dplyr::select(-"ft_sev")
  return(x)
}
