# Prepare `<incidence2>` data, basic expectations

    Code
      head(prepare_data(covid_uk_incidence, cases_variable = "cases_new",
        deaths_variable = "deaths_new", fill_NA = FALSE))
    Output
              date cases deaths
      1 2020-01-30    NA     NA
      2 2020-01-31    NA     NA
      3 2020-02-01    NA     NA
      4 2020-02-02    NA     NA
      5 2020-02-03    NA     NA
      6 2020-02-04    NA     NA

---

    Code
      tail(data)
    Output
                date cases deaths
      485 2021-05-28  6205      6
      486 2021-05-29  5146      5
      487 2021-05-30  5395      8
      488 2021-05-31  6251      6
      489 2021-06-01  3346      4
      490 2021-06-02     0      0

# Prepare grouped `<incidence2>` data

    Code
      head(prepare_data(data, cases_variable = "cases_new", deaths_variable = "deaths_new",
        fill_NA = FALSE))
    Output
              date          region cases deaths
      1 2020-01-30   East Midlands    NA     NA
      2 2020-01-30 East of England    NA     NA
      3 2020-01-30         England     2     NA
      4 2020-01-30          London    NA     NA
      5 2020-01-30      North East    NA     NA
      6 2020-01-30      North West    NA     NA

