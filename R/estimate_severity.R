#' @title Estimate the corrected case fatality risk
#'
#' @description Estimates the maximum likelihood estimate and 95% confidence
#' interval of a corrected severity, using the total cases and total cases with
#' known outcomes, where the latter replaces the total number of deaths in the
#' standard (naive) severity definition. We use a binomial likelihood,
#' approximated by a Poisson likelihood for large samples.
#'
#' @inheritParams cfr_static
#' @param total_cases The total number of cases observed over the period of an
#' outbreak of interest. The total number of cases must be greater than or equal
#' to the total number of deaths.
#' @param total_deaths The total number of deaths observed over the period of an
#' outbreak of interest. The total number of deaths must be less than or equal
#' to the total number of cases.
#' @param total_outcomes The total number of outcomes expected to be observed
#' over the period of an outbreak of interest. See [estimate_outcomes()].
#' @keywords internal
#' @return A `<data.frame>` with one row and three columns for the maximum
#' likelihood estimate and 95% confidence interval of the corrected severity
#' estimates, named "severity_mean", "severity_low", and "severity_high".
#'
#' @details
#' When any two of `total_cases`, `total_deaths`, or `total_outcomes` are zero,
#' the estimate and confidence intervals cannot be calculated and the output
#' `<data.frame>` contains `NA`s.
estimate_severity <- function(total_cases,
                              total_deaths,
                              total_outcomes,
                              poisson_threshold = 100) {
  # Add input checking for single numbers
  checkmate::assert_count(total_cases)
  checkmate::assert_number(total_deaths, upper = total_cases, lower = 0)
  # expect that the estimated number of outcomes is greater
  checkmate::assert_number(total_outcomes, lower = 0, finite = TRUE)
  checkmate::assert_count(poisson_threshold)

  # check for special case where any two of cases, deaths, and outcomes are zero
  if (sum(c(total_cases, total_deaths, total_outcomes) == 0) >= 2) {
    return(
      data.frame(
        severity_mean = NA_real_,
        severity_low = NA_real_,
        severity_high = NA_real_
      )
    )
  }

  # calculating the proportion of cases with known outcome
  u_t <- total_outcomes / total_cases

  # maximum likelihood estimation for corrected severity
  pprange <- seq(from = 1e-3, to = 1.0, by = 1e-3)

  # Calculate likelihood - use binomial for small samples and Poisson
  # approximation for larger numbers
  if (total_cases < poisson_threshold) {
    lik <- log(choose(round(u_t * total_cases), total_deaths)) +
      (total_deaths * log(pprange)) +
      (((u_t * total_cases) - total_deaths) * log(1.0 - pprange))
  } else {
    lik <- stats::dpois(total_deaths, pprange * u_t * total_cases, log = TRUE)
  }

  # maximum likelihood estimate - if this is empty, return NA
  severity_mean <- pprange[which.max(lik)]

  # 95% confidence interval of likelihood
  severity_lims <- range(pprange[lik >= (max(lik) - 1.92)])

  severity_estimate <- data.frame(
    severity_mean = severity_mean,
    severity_low = severity_lims[[1]],
    severity_high = severity_lims[[2]]
  )

  # returning vector with corrected estimates
  severity_estimate
}
