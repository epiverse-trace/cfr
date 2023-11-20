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

# Smooth inputs for static ascertainment

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1           0.729927               0.7          0.8313539

# Automatic burn-in for static ascertainment with delay correction

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1           0.729927               0.7          0.8313539

# Automatic burn-in for static ascertainment, no delay correction

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1           0.732906         0.7162026          0.7599719

# Static ascertainment from vignette

    Code
      estimate_ascertainment(data = covid_uk, delay_density = function(x) dlnorm(x,
        meanlog = 2.577, sdlog = 0.44), type = "static", severity_baseline = 0.014)
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1         0.06796117        0.06730769         0.06829268

# Basic expectations for time-varying ascertainment

    Code
      ascertainment_estimate
    Output
        ascertainment_mean ascertainment_low ascertainment_high
      1          0.2902857         0.2318857          0.3696896

