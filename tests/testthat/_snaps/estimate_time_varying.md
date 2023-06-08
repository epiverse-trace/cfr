# `estimate_time_varying`: Basic expectations

    Code
      head(tvcfr_naive, 15)
    Output
               date cases deaths severity_me severity_lo severity_hi
      1  1976-08-25     1      0         0.0  0.00000000   0.9750000
      2  1976-08-26     0      0          NA          NA          NA
      3  1976-08-27     0      0          NA          NA          NA
      4  1976-08-28     0      0          NA          NA          NA
      5  1976-08-29     0      0          NA          NA          NA
      6  1976-08-30     0      0          NA          NA          NA
      7  1976-08-31     0      0          NA          NA          NA
      8  1976-09-01     1      0         0.0  0.00000000   0.9750000
      9  1976-09-02     1      0         0.0  0.00000000   0.9750000
      10 1976-09-03     1      0         0.0  0.00000000   0.9750000
      11 1976-09-04     4      0         0.0  0.00000000   0.6023646
      12 1976-09-05     1      0         0.0  0.00000000   0.9750000
      13 1976-09-06     1      0         0.0  0.00000000   0.9750000
      14 1976-09-07     3      0         0.0  0.00000000   0.7075982
      15 1976-09-08     2      1         0.5  0.01257912   0.9874209

---

    Code
      tail(tvcfr_corrected, 15)
    Output
               date cases deaths severity_me severity_lo severity_hi
      59 1976-10-22     0      1         0.5  0.01257912   0.9874209
      60 1976-10-23     0      0         0.0  0.00000000   0.8418861
      61 1976-10-24     1      0         0.0  0.00000000   0.8418861
      62 1976-10-25     0      0         0.0  0.00000000   0.9750000
      63 1976-10-26     0      2          NA          NA          NA
      64 1976-10-27     0      0         0.0  0.00000000   0.9750000
      65 1976-10-28     0      2          NA          NA          NA
      66 1976-10-29     0      1         1.0  0.02500000   1.0000000
      67 1976-10-30     0      0         0.0  0.00000000   0.9750000
      68 1976-10-31     0      0         0.0  0.00000000   0.9750000
      69 1976-11-01     0      0          NA          NA          NA
      70 1976-11-02     0      0          NA          NA          NA
      71 1976-11-03     0      0          NA          NA          NA
      72 1976-11-04     0      0          NA          NA          NA
      73 1976-11-05     0      1          NA          NA          NA
