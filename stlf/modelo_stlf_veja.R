library(modeltime)
library(readxl)
library(tidyverse)
library(timetk)
library(tidymodels)
library(forecast)

serie_veja <- readr::read_rds("dados_p_graficos/base_veja.Rda") %>% 
  dplyr::rename(date = x, value = y) %>% 
  dplyr::mutate(
    date = lubridate::as_date(date),
    ano_semana = 100*lubridate::isoyear(date) + lubridate::isoweek(date)
  )

ts_serie_veja <-  stats::ts(
  data = serie_veja$value,
  start = c(2015,01), 
  frequency = 365.25/7
)

ano_mes_dia_final <- serie_veja$date %>% dplyr::last() %>% lubridate::ymd()
ano_mes_dia_final

h_horizonte <- 16

source("funcoes_modelos/func_semanas_foradaamostra.R")
weeks_foradamostra <- func_semanas_foradaamostra(
  h_semanas = h_horizonte,
  data_inicial = ano_mes_dia_final
)

# Preciso
ts_treino <- stats::ts(data = serie_veja[1:(nrow(serie_veja)-h_horizonte),]$value,
                       start = c(2015,01), frequency = 365.25/7)
teste <- serie_veja[(nrow(serie_veja)-(h_horizonte-1)):nrow(serie_veja),]$value

#Utilizando STLF (decompõe a série sazonalmente considerando os dados do 
#ano mais recente e estima um modelo de alisamento exponencial ETS)

#Previsão com os dados de treino num horizonte de 8 semanas à frente
ano_mes_dia_teste <- serie_veja[1:(nrow(serie_veja)-h_horizonte),]$date %>% 
  dplyr::last() %>% lubridate::ymd()

previsao_treino <- ts_treino %>% 
  forecast::stlf(h = h_horizonte) %>% 
  data.frame() %>% 
  cbind(teste, date = func_semanas_foradaamostra(ano_mes_dia_teste, h = h_horizonte)) %>% 
  dplyr::select(date, Point.Forecast, teste) %>% 
  dplyr::rename(.index = date, 
                PREVISAO_STLF = Point.Forecast,
                REAL = teste)
#Salvando os dados de teste
previsao_treino %>% 
  data.frame() %>% 
  readr::write_rds("dados-ajustados-teste/previsao-treino-stlf-veja.Rda")

# MAPE
previsao_treino %>% 
  dplyr::mutate(erros_absolutos = abs((REAL - PREVISAO_STLF)/REAL)) %>% 
  dplyr::summarise(mape = mean(erros_absolutos)) %>% 
  round(x=., digits = 3)*100

################################################################################
# 2.0 PREVISÃO

#Gerando uma sequência de datas semanais
source("funcoes_modelos/func_semanas_foradaamostra.R")
horizonte_previsao <- 16
# Último dia da semana na série original
ano_mes_dia_final <- serie_veja$date %>% dplyr::last() %>% lubridate::ymd()

previsao_final <- ts_serie_veja %>% 
  forecast::stlf(y = ., h = horizonte_previsao) %>% 
  data.frame() %>%
  dplyr::mutate(
    PREVISAO_STLF = round(Point.Forecast),
    .index = func_semanas_foradaamostra(
      h_semanas = horizonte_previsao, 
      data_inicial = ano_mes_dia_final),
    ano_semana = 100*lubridate::isoyear(.index) + lubridate::isoweek(.index)
  ) %>% 
  dplyr::select(.index, ano_semana, PREVISAO_STLF)

#Salvando arquivo final
previsao_final %>% readr::write_rds("previsao-modelos-stlf/previsao_veja.Rda")

rm(list=ls())
