fun_prep_decomp <- function(df) {
  
  df_ts <- df %>%
    dplyr::arrange(x)
  
  y_ts <- ts(df_ts$y, start = c(1, 1), frequency = 7)
  
  df_decomp <- y_ts %>%
    stl(s.window = "periodic", robust = TRUE) %>%
    magrittr::extract2("time.series") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      data = df_ts$x,
      wday = lubridate::wday(data, label = TRUE, week_start = 1)
    )

  df_decomp
  
}
