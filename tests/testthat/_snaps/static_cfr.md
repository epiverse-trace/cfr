# Basic expectations of static_cfr

    Code
      scfr_naive
    Output
         cfr_me   cfr_low  cfr_high 
      0.9551020 0.9210866 0.9773771 

---

    Code
      scfr_corrected
    Output
        cfr_me  cfr_low cfr_high 
         0.959    0.842    1.000 

---

    Code
      format_cfr_neatly(scfr_naive, type = "Naive")
    Output
                            CFR_estimate  Type
      1 95.51% (95% CI: 92.11% - 97.74%) Naive

---

    Code
      format_cfr_neatly(scfr_corrected, type = "Corrected")
    Output
                             CFR_estimate      Type
      1 95.90% (95% CI: 84.20% - 100.00%) Corrected

