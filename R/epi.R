#' Adjust severe rates as a result of treatment coverage and estimate mortality disaggregated by hospitalised and community
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
  ts <- 1 - treatment_scaler

  x <- x |>
    # Estimate hospitalised and severe incidence
    dplyr::mutate(
      # Estimate total severe from hospitalised severe based on original fitted model ft_sev of 0.8
      severe = .data$severe * (1 / 0.8),
      # Rescale for impact of first-line treatment (Assuming no difference in access by severity group)
      severe = severe * (.data$ft * ts + (1 - .data$ft)),
      # split total based on new estimate of ft_sev
      severe_hospital = severe * .data$ft_sev,
      severe_community = severe * (1 - .data$ft_sev)
    ) |>
    # Apply location-specific severe case fatality ratios to estimate mortality
    dplyr::mutate(
      mortality_hospital = .data$severe_hospital * hosp_sev_cfr,
      mortality_community = .data$severe_community * community_sev_cfr,
      mortality = .data$mortality_hospital + .data$mortality_community
    ) |>
    dplyr::select(-c("ft", "ft_sev"))
  return(x)
}
