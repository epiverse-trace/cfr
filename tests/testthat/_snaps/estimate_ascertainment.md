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
    Output
        ascertainment_estimate ascertainment_low ascertainment_high
      1              0.7297748         0.7147963          0.7530931

# Static ascertainment from vignette

    Code
      estimate_ascertainment(data = covid_uk, delay_density = function(x) dlnorm(x,
        meanlog = 2.577, sdlog = 0.44), severity_baseline = 0.014)
    Output
        ascertainment_estimate ascertainment_low ascertainment_high
      1             0.06779661        0.06734007         0.06829268

