#' Estimate the corrected case fatality rate
#'
#' @description Estimate the maximum likelihood estimate and 95% confidence
#' interval of a corrected severity, using the total cases and total cases with
#' known outcomes, where the latter replaces the total number of deaths in the
#' standard (naive) severity definition. We use a binomial likelihood,
#' approximated by a Poisson likelihood for large samples
#'
#' @inheritParams estimate_static
#' @keywords internal
#' @return A data.frame with the MLE and 95% confidence interval of the
#' corrected severity estimates, named "severity_me", "severity_low", and
#' "severity_high".
#'
estimate_severity <- function(df_in,
                              poisson_threshold = 100,
                              location) {

  # transferring from data.frame to vector format, to tidy up the slightly messy
  # likelihood calculation
  total_cases <- unique(df_in$total_cases)
  total_deaths <- unique(df_in$total_deaths)
  u_t <- unique(df_in$u_t)

  stopifnot(
    "`total_cases` must be equal to or more than `total_deaths`" =
      (total_cases >= total_deaths)
  )

  # MLE estimation for corrected severity
  pprange <- seq(from = 1e-3, to = 1.0, by = 1e-3)

  # Calculate likelihood - use binomial for small samples and Poisson
  # approximation for larger numbers
  lik <- numeric()
  if (total_cases < poisson_threshold) {
    lik <- log(choose(round(u_t * total_cases), total_deaths)) +
      (total_deaths * log(pprange)) +
      (((u_t * total_cases) - total_deaths) * log(1.0 - pprange))
  } else {
    lik <- stats::dpois(total_deaths, pprange * u_t * total_cases, log = TRUE)
  }

  # MLE estimate
  severity_me <- pprange[which.max(lik)]

  # 95% range of likelihood
  severity_lims <- range(pprange[lik >= (max(lik) - 1.92)])

  if (!is.null(location) && location %in% colnames(df_in)) {
    severity_estimate <- data.frame(
      "location" = unique(df_in[[location]]),
      "severity_me" = severity_me,
      "severity_lo" = severity_lims[[1]],
      "severity_hi" = severity_lims[[2]]
    )
  } else {
    severity_estimate <- data.frame(
      "severity_me" = severity_me,
      "severity_lo" = severity_lims[[1]],
      "severity_hi" = severity_lims[[2]]
    )
  }

  severity_estimate[is.na(severity_estimate)] <- NA_real_

  # returning vector with corrected estimates
  return(severity_estimate)
}
