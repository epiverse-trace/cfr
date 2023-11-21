# Basic expectations for static ascertainment

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1           0.732906         0.7162026          0.7599719

# Correct for delays for static ascertainment

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1           0.729927               0.7          0.8313539

# Static ascertainment from vignette

    Code
      estimate_ascertainment(data = covid_uk, delay_density = function(x) dlnorm(x,
        meanlog = 2.577, sdlog = 0.44), severity_baseline = 0.014)
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1         0.06796117        0.06730769         0.06829268

