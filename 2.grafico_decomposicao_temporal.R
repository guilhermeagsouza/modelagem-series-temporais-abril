library(fpp2)
library(fpp3)
library(tidyverse)
library(readr)

################################################################################
# SITE 
base_claudia <- readr::read_rds("dados_p_graficos/base_claudia.Rda")
df_consolidado <- readxl::read_xlsx(
  "dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")

ano_semana <- df_consolidado %>% 
  dplyr::mutate(isoYearIsoWeek = as.numeric(isoYearIsoWeek)) %>% 
  dplyr::filter(dia_da_semana == "seg") %>% 
  group_by(isoYearIsoWeek) %>% 
  summarise(data = dplyr::first(date))

ts_claudia <- stats::ts(data = base_claudia$y, 
          frequency = 365.25/7,
          start = c(2017,01)
)


dcmp <- decompose(ts_claudia)

decomposicao_temporal <- data.frame(
  data = base_claudia$x,
  Sessions = dcmp$x,
  Tendencia = dcmp$trend,
  Sazonalidade = dcmp$seasonal,
  Aleatorio = dcmp$random
) %>% 
  dplyr::left_join(ano_semana, by = c("data"))

decomposicao_temporal %>% 
  readr::write_rds("decomposicao_temporal/decomposicao_temporal_claudia.Rda")

################################################################################