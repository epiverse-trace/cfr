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
#' ## Special cases
#'
#' - When any two of `total_cases`, `total_deaths`, or `total_outcomes` are 0,
#' the estimate and confidence intervals cannot be calculated and the output
#' `<data.frame>` contains only `NA`s.
#'
#' - When `total_cases == total_deaths` _and_ `total_outcomes <= total_deaths`,
#' while `total_cases < poisson_threshold`, the confidence intervals cannot be
#' calculated and are returned as `NA`. The severity is returned as the lowest
#' possible value for the method used when cases are below the Poisson
#' threshold, which is 0.001.
#'
#' - When `total_outcomes == total_deaths` while
#' `total_cases < poisson_threshold` the confidence intervals cannot be
#' calculated and are returned as `NA`s while the severity estimate is returned
#' as `0.999`.
estimate_severity <- function(total_cases,
                              total_deaths,
                              total_outcomes,
                              poisson_threshold = 100) {
  # Add input checking for single numbers
  checkmate::assert_count(total_cases)
  # use assert_number to set upper limit at total_cases
  checkmate::assert_number(total_deaths, upper = total_cases, lower = 0)
  # expect that the estimated number of outcomes is greater
  # need not be integer-like, need not be limited by cases or deaths
  checkmate::assert_number(total_outcomes, lower = 0, finite = TRUE)
  checkmate::assert_count(poisson_threshold, positive = TRUE)

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
    lik <- lchoose(round(u_t * total_cases), total_deaths) +
      (total_deaths * log(pprange)) +
      (((u_t * total_cases) - total_deaths) * log(1.0 - pprange))
  } else {
    lik <- stats::dpois(
      total_deaths, pprange * round(u_t * total_cases),
      log = TRUE
    )
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

#' @title Select a likelihood function for severity estimation
#' @description
#' Switches between Binomial, Poisson, and Normal approximation based on the
#' total number of cases and an initial estimate of the severity.
#'
#' @param total_cases A single count for the total number of cases in the
#' outbreak.
#' @param poisson_threshold A single count for the threshold of cases above
#' which a Poisson or Normal approximation is returned.
#' @param p_mid A single positive number bounded 0 -- 1, representing an initial
#' estimate of the severity, which is used to determine whether a Poisson or
#' Normal approximation is returned.
#' determine whether
#' @details
#' Returns a likelihood function as follows:
#'
#' - Binomial approximation: when `total_cases < poisson_threshold`,
#' used when there are few cases, such as in a small outbreak;
#'
#' - Poisson approximation: when `total_cases >= poisson_threshold` but
#' when `p_mid` < 0.05;
#'
#' - Normal approximation: when `total_cases >= poisson_threshold` and
#' `p_mid >=` 0.05.
#'
#' @return A function with three arguments, `total_outcomes`, `total_deaths`,
#' and `pp`, which is used to generate the profile likelihood.
#' Also prints messages to the screen when a Poisson or Normal approximation
#' function is returned.
#' @keywords internal
.select_func_likelihood <- function(total_cases, poisson_threshold, p_mid) {
  # NOTE: internal function is not input checked
  # switch likelihood function based on total cases and p_mid
  # Binomial approx
  if (total_cases < poisson_threshold) {
    func_likelihood <- function(total_outcomes, total_deaths, pp) {
      lchoose(round(total_outcomes), total_deaths) +
        (total_deaths * log(pp)) +
        (((total_outcomes) - total_deaths) * log(1.0 - pp))
    }
  }

  # Poisson approx
  if ((total_cases >= poisson_threshold) && p_mid < 0.05) {
    func_likelihood <- function(total_outcomes, total_deaths, pp) {
      stats::dpois(
        total_deaths, pp * round(total_outcomes),
        log = TRUE
      )
    }
    message(
      "Total cases = ", total_cases, " and p = ", signif(p_mid, 3),
      ": using Poisson approximation to binomial likelihood."
    )
  }

  # Normal approx
  if ((total_cases >= poisson_threshold) && p_mid >= 0.05) {
    func_likelihood <- function(total_outcomes, total_deaths, pp) {
      stats::dnorm(
        total_deaths,
        mean = pp * round(total_outcomes),
        sd = pp * (1 - pp) * round(total_outcomes),
        log = TRUE
      )
    }
    message(
      "Total cases = ", total_cases, " and p = ", signif(p_mid, 3),
      ": using Normal approximation to binomial likelihood."
    )
  }

  func_likelihood
}
