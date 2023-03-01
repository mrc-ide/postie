#' Add mortality rate
#'
#' @param x output
#' @param scaler Scaler for severe cases to deaths. The
#' original fitted scaler for P. falciparum is 0.215. See
#' \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}.
#'
#' @return output with mortality rates
#' @export
mortality_rate <- function(x, scaler){
  if(scaler > 1 || scaler < 0){
    stop("scaler must be between 0 and 1")
  }

  x <- x |>
    dplyr::mutate(mortality = scaler * .data$severe)
  return(x)
}

#' Adjust severe (and downstream mortality) rates as a result of treatment coverage.
#'
#' @param x Input data.frame
#' @param treatment_scaler The impact of treatment coverage on progression to severe disease and death.
#' A highly uncertain parameter, the analysis by
#'  \href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}
#' sampled from a uniform diastribution (0, 1).
#' @param baseline_treatment The proportion of uncomplicated malaria cases that are effectively treated historically.
treatment_scaling <- function(x, treatment_scaler, baseline_treatment = 0){
  if(treatment_scaler > 1 || treatment_scaler < 0){
    stop("treatment_scaler must be between 0 and 1")
  }
  if(baseline_treatment > 1 || baseline_treatment < 0){
    stop("treatment_scaler must be between 0 and 1")
  }
  ts <- (1 - treatment_scaler)
  x <- x |>
    dplyr::mutate(severe = .data$severe * ((ts * .data$ft + (1 - .data$ft)) / (ts * baseline_treatment + (1 - baseline_treatment)))) |>
    dplyr::select(-"ft")
  return(x)
}
