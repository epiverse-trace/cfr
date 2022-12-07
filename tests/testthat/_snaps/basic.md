# Basic tests from examples

    Code
      ncfr
    Output
         cfr_me   cfr_low  cfr_high 
      0.9551020 0.9210866 0.9773771 

---

    Code
      ccfr
    Output
        cfr_me  cfr_low cfr_high 
         0.970    0.851    1.000 

---

    Code
      format_cfr_neatly(ncfr)
    Output
      [1] "CFR: 0.955% (95% Ci: 0.921% -- 0.977%)"

---

    Code
      format_cfr_neatly(ccfr)
    Output
      [1] "CFR: 0.970% (95% Ci: 0.851% -- 1.000%)"

