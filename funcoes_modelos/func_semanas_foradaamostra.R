library(magrittr)
library(lubridate)
func_semanas_foradaamostra <- function(h_semanas, data_inicial) {
  
  data_inicial <- data_inicial %>% 
    lubridate::as_date() %>% lubridate::ymd()
  
  semanas_foradaamostra <- seq(
    from = ISOdate(
      lubridate::year(data_inicial),
      lubridate::month(data_inicial),
      lubridate::day(data_inicial)
    ),
    by = "week",
    length.out = (h_semanas + 1)
  )
  semanas_foradaamostra %<>%  lubridate::as_date() %<>% lubridate::ymd()
  return(semanas_foradaamostra[-1])
  
}

func_ano_semana <- function(date){
  assertthat::is.date(date)
  100*lubridate::isoyear(date) + lubridate::isoweek(date)
}
