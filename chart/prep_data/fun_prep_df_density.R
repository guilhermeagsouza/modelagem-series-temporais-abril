fun_prep_df_density <- function(df, mes_referencia, mes_analise) {
  
  df %>%
    dplyr::mutate(ano_mes = stringr::str_sub(x, 1, 7)) %>%
    dplyr::filter(ano_mes %in% c(mes_referencia, mes_analise)) %>%
    dplyr::select(ano_mes, y) %>%
    tidyr::nest(data = c(y)) %>%
    dplyr::mutate(
      classe = dplyr::case_when(
        ano_mes == mes_referencia ~ "referência",
        ano_mes == mes_analise ~    "análise",
        NA ~ "outro"
      ), 
      
      densidade = purrr::map(
        .x = data,
        .f = function(x) {
          d <- density(x$y)
          tibble::tibble(x = d$x, y = d$y)
        }
      )
    ) %>%
    dplyr::select(ano_mes, classe, densidade) %>%
    tidyr::unnest(densidade)
  
}