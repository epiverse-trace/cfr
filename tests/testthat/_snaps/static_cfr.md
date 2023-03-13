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
      format_cfr_neatly(scfr_naive)
    Output
      [1] "CFR: 0.96% (95% CI: 0.92% - 0.98%)"

---

    Code
      format_cfr_neatly(scfr_corrected)
    Output
      [1] "CFR: 0.96% (95% CI: 0.84% - 1.00%)"

