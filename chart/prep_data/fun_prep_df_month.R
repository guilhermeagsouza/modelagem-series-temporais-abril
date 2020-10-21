fun_prep_df_month <- function(df) {
  
  # packages ----
  require(assertthat)
  require(magrittr)
  require(tidyverse)
  require(lubridate)
  require(highcharter)
  
  
  df_month <- df %>%
    dplyr::group_by(
      year = lubridate::year(x),
      month = lubridate::month(x)
    ) %>%
    dplyr::summarise(
      min_x = min(x),
      max_x = max(x),
      avg   = round(mean(y)),
      cv    = sd(y) / avg
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      max_x_ref = stringr::str_c(
        year, stringr::str_pad(string = month, width = 2, pad = "0"), "01"
      ) %>%
        lubridate::ymd() %>%
        lubridate::add_with_rollback(months(1)) %>%
        lubridate::add_with_rollback(lubridate::days(-1))
    ) %>%
    dplyr::filter(lubridate::day(min_x) == 1, max_x == max_x_ref) %>%
    dplyr::select(min_x, max_x, y = avg)
  
  
  df_month %>%
    dplyr::mutate(
      min_x_num = min_x %>%
        highcharter::datetime_to_timestamp(),
      max_x_num = max_x %>%
        lubridate::add_with_rollback(lubridate::days(1)) %>%
        highcharter::datetime_to_timestamp() %>%
        magrittr::add(-1),
      name = min_x %>%
        format("%b %y") %>%
        stringr::str_to_title(),
      coord_polygon = purrr::pmap(
        .l = list(min_x_num, max_x_num, y),
        .f = fun_calc_coord_polygon
      ),
      x_avg = (min_x_num + max_x_num) / 2,
      y_tooltip = y %>% round(0) %>% format(nsmall = 0, big.mark = " ")
    )
  
}