#' Estimate the corrected case fatality rate
#'
#' @description Estimate the maximum likelihood estimate and 95% confidence
#' interval of a corrected CFR, using the total cases and total cases with known
#' outcomes, where the latter replaces the total number of deaths in the
#' standard (naive) CFR definition. We use a binomial likelihood, approximated
#' by a Poisson likelihood for large samples
#'
#' @param total_cases The total number of cases observed over the period of an
#' outbreak of interest. The total number of cases must be greater than or equal
#' to the total number of deaths
#'
#' @param total_deaths The total number of deaths observed over the period of an
#' outbreak of interest. The total number of deaths must be less than or equal
#' to the total number of cases
#'
#' @param u_t The proportion of cases to cases with known outcomes up to the
#' point of an outbreak of interest. Used to correct the total number of deaths
#' for delays between case detection and outcome. Given that its a proportion,
#' it must be between 0.0 and 1.0.
#' @inheritParams static_cfr
#'
#' @return A named vector with the MLE and 95% confidence interval of the
#' corrected CFR estimates, named "cfr_me", "cfr_low", and "cfr_high".
#' @export
#'
#' @examples
#' estimate_ccfr(total_cases = 1000, total_deaths = 900, u_t = 0.2)
estimate_ccfr <- function(total_cases,
                          total_deaths,
                          u_t,
                          poisson_threshold = 200) {
  stopifnot(
    "`total_cases` must be equal to or more than `total_deaths`" =
      (total_cases >= total_deaths)
  )

  # MLE estimation for corrected CFR
  pprange <- seq(from = 1e-3, to = 1.0, by = 1e-3)

  # Calculate likelihood - use binomial for small samples and Poisson
  # approximation for larger numbers
  lik <- numeric()
  if (total_cases < poisson_threshold) {
    lik <- log(choose(total_cases, total_deaths)) +
      (total_deaths * log(pprange)) +
      ((total_cases - total_deaths) * log(1 - pprange))
  } else {
    lik <- stats::dpois(total_deaths, pprange * u_t * total_cases, log = TRUE)
  }

  # MLE estimate
  cfr_me <- pprange[which.max(lik)]

  # 95% range of likelihood
  cfr_lims <- range(pprange[lik >= (max(lik) - 1.92)])

  # putting together the estimates
  cfr_estimate <- c(cfr_me, cfr_lims)
  names(cfr_estimate) <- c("cfr_me", "cfr_low", "cfr_high")

  if (is.na(max(lik))) {
    cfr_estimate <- rep(NA_real_, length(cfr_estimate)) # set all to NA
  }

  # returning vector with corrected estimates
  cfr_estimate
}
