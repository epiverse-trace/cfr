# `plot_data` function basic functionality

    Code
      this_plot$data
    Output
           cfr_me cfr_low cfr_high       date cases deaths type
        1:  0.000   0.000    0.975 1976-08-25     1      0 ncfr
        2:  0.000   0.000    0.975 1976-08-26     0      0 ncfr
        3:  0.000   0.000    0.975 1976-08-27     0      0 ncfr
        4:  0.000   0.000    0.975 1976-08-28     0      0 ncfr
        5:  0.000   0.000    0.975 1976-08-29     0      0 ncfr
       ---                                                     
      142:  0.966   0.847    1.000 1976-11-01     0      0 ccfr
      143:  0.966   0.847    1.000 1976-11-02     0      0 ccfr
      144:  0.966   0.847    1.000 1976-11-03     0      0 ccfr
      145:  0.966   0.847    1.000 1976-11-04     0      0 ccfr
      146:  0.970   0.851    1.000 1976-11-05     0      1 ccfr

---

    Code
      this_plot$layers
    Output
      [[1]]
      mapping: x = ~date, ymin = ~cfr_low, ymax = ~cfr_high, fill = ~type 
      geom_ribbon: na.rm = FALSE, orientation = NA, outline.type = both
      stat_identity: na.rm = FALSE
      position_identity 
      
      [[2]]
      mapping: x = ~date, y = ~cfr_me, colour = ~type 
      geom_line: na.rm = FALSE, orientation = NA
      stat_identity: na.rm = FALSE
      position_identity 
      

