fun_prep_df_ma <- function(df, ma = c(30)) {
  
  # packages ----
  require(assertthat)
  require(magrittr)
  require(tidyverse)
  require(highcharter)
  
  
  
  # assert ----
  assertthat::assert_that(is.data.frame(df))
  # assertthat::assert_that(is.character(df$x))
  assertthat::assert_that(is.numeric(df$y))
  
  
  
  # MA config
  names_ma <- stringr::str_c("ma", ma)
  
  list_funs <- purrr::map(
    .x = ma,
    .f = ~purrr::partial(.f = forecast::ma, order = .x)
  ) %>%
    magrittr::set_names(names_ma)
  
  df %>%
    dplyr::mutate_at(.vars = "y", .funs = list_funs) %>%
    tidyr::pivot_longer(cols = -x) %>%
    
    # Remove ma missing
    na.omit() %>%
    
    dplyr::filter(name != "y") %>%
    
    # Transform factor
    dplyr::mutate(
      x = highcharter::datetime_to_timestamp(x),
      name = name %>% factor(c("y", names_ma)),
      value = round(value)
    ) %>%
    
    dplyr::rename(y = value)
  
}