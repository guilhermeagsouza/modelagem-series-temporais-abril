fun_prep_df_seasonal <- function(df) {
  
  colfunc <- grDevices::colorRampPalette(
    c(
      "#cb4335", "#e74c3c", "#ec7063", "#f1948a", "#f5b7b1", "#fadbd8", "#fdedec",
      "#e8f8f5", "#d1f2eb", "#a3e4d7", "#76d7c4", "#48c9b0", "#1abc9c", "#17a589"
    )
  )
  
  df_color <- tibble::tibble(
    valor_chr = seq(from = -1,to = 1, by = 0.01) %>%
      round(2) %>%
      format(nsmall = 2) %>%
      stringr::str_trim(),
    cor = colfunc(201)
  )
  
  df_seasonal <- df %>%
    fun_prep_decomp() %>%
    dplyr::mutate(inicio = cumsum(wday == "seg")) %>%
    dplyr::filter(inicio == 1) %>%
    dplyr::select(wday, seasonal) %>%
    dplyr::mutate(
      seasonal_std = round(seasonal / max(abs(seasonal)), 2),
      valor_chr = seasonal_std %>%
        round(2) %>%
        format(nsmall = 2) %>%
        stringr::str_trim()
    ) %>%
    dplyr::left_join(y = df_color, by = "valor_chr") %>%
    dplyr::mutate(wday_name = wday %>% levels()) %>%
    dplyr::select(wday_name, seasonal_std, cor)
  
  
  df_seasonal
  
}