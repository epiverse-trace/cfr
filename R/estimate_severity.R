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
#' @param p_mid The initial severity estimate, which is used to determine the
#' likelihood approximation used when `total_cases` > `poisson_threshold`.
#' Defaults to `total_deaths / round(total_outcomes)`.
#' @keywords internal
#' @return A `<data.frame>` with one row and three columns for the maximum
#' likelihood estimate and 95% confidence interval of the corrected severity
#' estimates, named "severity_estimate", "severity_low", and "severity_high".
#'
#' @details
#' ## Special cases
#'
#' - When any two of `total_cases`, `total_deaths`, or `total_outcomes` are 0,
#' the estimate and confidence intervals cannot be calculated and the output
#' `<data.frame>` contains only `NA`s.
#'
#' - When `total_outcomes <= total_deaths`, estimate and confidence intervals
#' cannot be reliably calculated and are returned as `NA`.
.estimate_severity <- function(total_cases,
                               total_deaths,
                               total_outcomes,
                               poisson_threshold,
                               p_mid = total_deaths / round(total_outcomes)) {
  # NOTE: no input checking on internal fn

  # check for special case where any two of cases, deaths, and outcomes are zero
  # NOTE: total_cases needed only here
  if (sum(c(total_cases, total_deaths, total_outcomes) == 0) >= 2) {
    return(
      c(
        severity_estimate = NA_real_,
        severity_low = NA_real_,
        severity_high = NA_real_
      )
    )
  }

  # NOTE: previous code used `u_t = total_outcomes / total_cases`
  # which can be simplified in all operations to simply `total_outcomes`

  # select likelihood function
  func_likelihood <- .select_func_likelihood(
    total_cases, poisson_threshold, p_mid
  )

  # maximum likelihood estimation for corrected severity
  # using increments of 0.1% severity
  pprange <- seq(from = 1e-4, to = 1.0, by = 1e-4)

  # if more expected outcomes than observed deaths, set outcomes equal to deaths
  if (total_outcomes >= total_deaths){ 
      total_outcomes_checked <- total_outcomes
      }else{ 
      total_outcomes_checked <- NA
      message(
        "Total deaths = ", total_deaths,
        " and expected outcomes = ", round(total_outcomes),
        " so setting expected outcomes = NA. If we were to assume 
        total deaths = expected outcomes, it would produce an estimate of 1."
      )
  }
  
  # get likelihoods using selected function
  lik <- func_likelihood(total_outcomes_checked, total_deaths, pprange)

  # maximum likelihood estimate - if this is empty, return NA
  # Otherwise return 95% confidence interval of likelihood
  severity_estimate <- pprange[which.max(lik)]
  if (length(severity_estimate)==0){
    severity_estimate <- NA
    severity_lims <- c(NA, NA)
    }else{
    severity_lims <- range(pprange[lik >= 
                                     (max(lik, na.rm = TRUE) - 1.92)],
                           na.rm = TRUE)
  }

  # return a vector for easy conversion to data
  severity_estimate <- c(severity_estimate, severity_lims)
  names(severity_estimate) <- sprintf(
    "severity_%s", c("estimate", "low", "high")
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
#' @return A function with three arguments, `total_outcomes`, `total_deaths`,
#' and `pp`, which is used to generate the profile likelihood.
#' Also prints messages to the screen when a Poisson or Normal approximation
#' function is returned.
#' @keywords internal
.select_func_likelihood <- function(total_cases, poisson_threshold, p_mid) {
  # NOTE: internal function is not input checked
  # switch likelihood function based on total cases and p_mid
  # Binomial approx
  if (total_cases < poisson_threshold  || (p_mid >= 0.05)) {
    func_likelihood <- function(total_outcomes, total_deaths, pp) {
      lchoose(round(total_outcomes), total_deaths) +
        (total_deaths * log(pp)) +
        (((total_outcomes) - total_deaths) * log(1.0 - pp))
    }
  }

  # Poisson approx
  if ((total_cases >= poisson_threshold) && (p_mid < 0.05)) {
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

  func_likelihood
}
