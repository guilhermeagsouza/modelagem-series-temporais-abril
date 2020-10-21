library(tidyverse)
library(magrittr)
library(readr)

#FUNÇÕES

###############################################################################
fun_calc_pesos <- function() {
  
  peso1 <- peso2 <- peso3 <- seq(from = 0, to = 1, by = 0.01)
  
  tidyr::expand_grid(peso1 = peso1, peso2 = peso2, peso3 = peso3) %>%
    dplyr::filter(peso1 + peso2 + peso3 == 1)
  
}

fun_calc_mape <- function(peso1, peso2, peso3, df_previsoes) {
  
  previsao_ensemble <- 
    (df_previsoes$y_ajustado_modelo1 * peso1) +
    (df_previsoes$y_ajustado_modelo2 * peso2) +
    (df_previsoes$y_ajustado_modelo3 * peso3)
  
  mean(abs((df_previsoes$y_real - previsao_ensemble) / df_previsoes$y_real))
  
}


fun_ensemble <- function(df_previsoes) {
  
  fun_calc_pesos() %>%
    dplyr::mutate(
      mape = purrr::pmap_dbl(
        .l = .,
        .f = fun_calc_mape,
        df_previsoes = df_previsoes
      )
    ) %>%
    dplyr::arrange(mape)
  
}
###############################################################################

df_ajustados_STLF <- readr::read_rds("dados-ajustados-teste/previsao-treino-stlf-boaforma.Rda") %>% 
  dplyr::rename(y_ajustado_modelo1 = PREVISAO_STLF)

df_ajustados_SARIMA <- readr::read_rds("dados-ajustados-teste/previsao-treino-sarima-boaforma.Rda") %>% 
  dplyr::rename(y_ajustado_modelo2 = PREVISAO_SARIMA)

df_ajustados_PROPHET <- readr::read_rds("dados-ajustados-teste/previsao-treino-prophet-boaforma.Rda") %>% 
  dplyr::rename(y_ajustado_modelo3 = PREVISAO_PROPHET)

df_ensemble <- df_ajustados_STLF %>% 
  dplyr::left_join(df_ajustados_SARIMA, by = c(".index","REAL")) %>% 
  dplyr::left_join(df_ajustados_PROPHET, by = c(".index","REAL")) %>% 
  dplyr::rename(y_real = REAL) %>% 
  dplyr::select(-.index) %>% 
  fun_ensemble()

pesos <- df_ensemble %>% 
  slice(1)
pesos

pesos %>% readr::write_rds("previsao-modelos-ensemble/pesos_ensemble_boaforma.Rda")
################################################################################

previsao_stlf <- readr::read_rds("previsao-modelos-stlf/previsao_boaforma.Rda")
previsao_sarima <- readr::read_rds("previsao-modelos-sarima/previsao_boaforma.Rda")
previsao_prophet <- readr::read_rds("previsao-modelos-prophet/previsao_boaforma.Rda")

#Criando o ensemble
modelo_ensemble <- previsao_stlf %>% 
  dplyr::left_join(previsao_sarima, by = c(".index", "ano_semana")) %>% 
  dplyr::left_join(previsao_prophet, by = c(".index","ano_semana")) %>% 
  dplyr::mutate(
    modelo_ensemble = PREVISAO_STLF*pesos$peso1 + PREVISAO_PROPHET*pesos$peso2 + PREVISAO_SARIMA*pesos$peso3,
    MAPE_ensemble = pesos$mape
  ) %>% 
  dplyr::select(-ano_semana)
modelo_ensemble

modelo_ensemble %>% 
  readr::write_rds("previsao-modelos-ensemble/modelo_ensemble_boaforma.Rda")

rm(list=ls())
