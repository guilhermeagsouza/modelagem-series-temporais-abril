library(readxl)
library(tidyverse)
library(magrittr)
library(lubridate)

func_create_df_feriado <- function(weeks_foradaamostra) {
  
  df_feriado <- readxl::read_xlsx("dados_externos/feriados_anbima.xlsx") %>% 
    mutate(
      ano_semana = 100*lubridate::isoyear(.index) + lubridate::isoweek(.index),
      feriado = 1
    ) %>% 
    dplyr::select(ano_semana, feriado) %>% 
    dplyr::distinct()
  
  feriados_fora_amostra <- data.frame(x = weeks_foradamostra) %>% 
    dplyr::mutate(ano_semana = 100*lubridate::isoyear(x) + lubridate::isoweek(x)) %>% 
    dplyr::left_join(y = df_feriado, by = c("ano_semana")) %>% 
    dplyr::mutate(feriado = ifelse(test = !is.na(feriado), yes = 1, no = 0))
  
  return(feriados_fora_amostra)
}
