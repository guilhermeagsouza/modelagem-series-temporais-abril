fun_prep_df_point <- function(df) {
  
  # package ----
  require(assertthat)
  require(tidyverse)
  require(anomalize)
  require(highcharter)
  
  
  
  # assert ----
  assertthat::assert_that(is.data.frame(df))
  
  
  
  # prep df ----
  df %>%
    dplyr::arrange(x) %>%
    anomalize::time_decompose(y, method = "stl") %>%
    anomalize::anomalize(remainder, method = "iqr") %>%
    dplyr::select(x, y = observed, anomaly) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      anomaly = anomaly %>%
        dplyr::recode(
          "No"  = "Série",
          "Yes" = "Outlier"
        ) %>%
        factor(c("Série", "Outlier")),
      x_tooltip = x %>% format("%d %b %y"),
      y_tooltip = y %>% round(0) %>% format(nsmall = 0, big.mark = " "),
      x = highcharter::datetime_to_timestamp(x)
    ) %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(break_factor = fun_break_point(y)) %>%
    tidyr::nest(data = -c(break_factor)) %>%
    dplyr::mutate(
      basal = purrr::map(.x = data, .f = fun_basal)
    ) %>%
    tidyr::unnest(cols = c(data, basal))
  
}





