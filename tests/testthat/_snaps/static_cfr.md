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
         0.957    0.840    1.000 

---

    Code
      format_cfr_neatly(scfr_naive)
    Output
      [1] "CFR: 0.955% (95% Ci: 0.921% -- 0.977%)"

---

    Code
      format_cfr_neatly(scfr_corrected)
    Output
      [1] "CFR: 0.957% (95% Ci: 0.840% -- 1.000%)"

