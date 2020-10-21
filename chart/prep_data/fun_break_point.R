fun_break_point <- function(y) {
  
  serie <- ts(y)
  
  break_points = strucchange::breakpoints(serie ~ 1)
  break_points %>%
    strucchange::breakfactor() %>%
    as.character()
  
}
