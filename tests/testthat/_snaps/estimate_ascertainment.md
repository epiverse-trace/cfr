# Basic expectations for static ascertainment

    Code
      estimate_ascertainment(data = ebola1976, severity_baseline = 0.7)
    Output
        ascertainment_estimate ascertainment_low ascertainment_high
      1               0.732906         0.7162026          0.7599719

# Correct for delays for static ascertainment

    Code
      estimate_ascertainment(data = ebola1976, delay_density = function(x) dgamma(x,
        shape = 2.4, scale = 3.33), severity_baseline = 0.7)
    Message
      Total cases = 245 and p = 0.959: using Normal approximation to binomial likelihood.
    Output
        ascertainment_estimate ascertainment_low ascertainment_high
      1              0.7185383         0.7087172          0.8377214

# Static ascertainment from vignette

    Code
      estimate_ascertainment(data = covid_uk, delay_density = function(x) dlnorm(x,
        meanlog = 2.577, sdlog = 0.44), severity_baseline = 0.014)
    Message
      Total cases = 283420 and p = 0.206: using Normal approximation to binomial likelihood.
    Output
        ascertainment_estimate ascertainment_low ascertainment_high
      1             0.09810792        0.02316347          0.2167183

