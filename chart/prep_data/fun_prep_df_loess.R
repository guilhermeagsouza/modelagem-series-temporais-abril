fun_prep_df_loess <- function(df, span = 0.1) {
  
  # packages ----
  require(assertthat)
  require(magrittr)
  require(tidyverse)
  require(highcharter)
  
  
  
  # assert ----
  assertthat::assert_that(is.data.frame(df))
  # assertthat::assert_that(is.character(df$x))
  assertthat::assert_that(is.numeric(df$y))
  
  
  
  # loess
  l <- round(predict(loess(df$y~seq_along(df$y), span = span)))
  
  df %>%
    dplyr::mutate(
      x = highcharter::datetime_to_timestamp(x),
      y = l,
      name = "loess"
    )
  
}