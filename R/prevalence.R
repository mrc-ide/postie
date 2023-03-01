#' Extract basic rates from model output
#'
#' @param x Input data.frame
#' @inheritParams prevalence_format
#' @inheritParams time_transform
#'
#' @export
get_prevalence <- function(x, time_divisor, baseline_t, age_divisor){
  prevalence <- x |>
    prevalence_estimate() |>
    time_transform(time_divisor = time_divisor, baseline_t = baseline_t) |>
    prevalence_time_aggregate() |>
    prevalence_format(age_divisor = age_divisor)
  return(prevalence)
}

#' Extract prevalence estimates from malariasimulation output
#'
#' @param x Input data.frame
prevalence_estimate <- function(x){
  prevalence_cols = colnames(x)[grepl("n_detect", colnames(x))]
  prevalence_denominator_cols = stringr::str_replace(prevalence_cols, "n_detect", "n")
  prevalence <- x[ , prevalence_cols, drop = FALSE] / x[ , prevalence_denominator_cols]
  x <- dplyr::bind_cols(timestep = x[,"timestep"], prevalence)
  return(x)
}

#' Aggregate prevalence over time
#'
#' @param x Input data.frame
prevalence_time_aggregate <- function(x){
  x <- x |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), mean),
      .by = "t"
    )
  return(x)
}

#' Format prevalence output
#'
#' @param x Input datas.frame
#' @param age_divisor Aggregation level. For example setting to 365 will return
#' age units in years
prevalence_format <- function(x, age_divisor = 365){
  if(age_divisor < 1){
    stop("age_divisor must be > 1")
  }

  cols <- colnames(x[,])
  age_ranges <- stringr::str_split(stringr::str_replace(cols[!cols == "t"], "n_detect_", ""), "_")
  cols[!cols == "t"] <- sapply(age_ranges, function(x){
    transformed_age <- round(as.numeric(x) / age_divisor, 2)
    paste0("prevalence_", paste0(transformed_age, collapse = "_"))
  })
  colnames(x) <- cols
  return(x)
}
