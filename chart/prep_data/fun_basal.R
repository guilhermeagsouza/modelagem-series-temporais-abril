fun_break_point <- function(y) {
  
  serie <- ts(y)
  
  break_points = strucchange::breakpoints(serie ~ 1) #2 breakpoints at 30 and 80
  break_points %>%
    strucchange::breakfactor() %>%
    as.character()
  
}


fun_basal <- function(df) {
  
  df_aux <- df %>%
    dplyr::select(x, y, anomaly) %>%
    dplyr::mutate(x = (x - min(x)) / 86400000)
  
  model <- df_aux %>%
    dplyr::filter(anomaly == "Série") %>%
    lm(y~x, data = .)
  
  predict(model, df_aux)
  
}