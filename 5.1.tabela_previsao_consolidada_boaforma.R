#Dados com todos os modelos
tbl_teste <- readr::read_rds("dados-modelo-teste/boaforma.Rda") 

STLF_teste <- readr::read_rds("dados-ajustados-teste/previsao-treino-stlf-boaforma.Rda") %>% 
  dplyr::rename(ATUAL = REAL, STLF = PREVISAO_STLF)

SARIMA_teste <- readr::read_rds("dados-ajustados-teste/previsao-treino-sarima-boaforma.Rda") %>% 
  dplyr::rename(ATUAL = REAL, SARIMA = PREVISAO_SARIMA)

PROPHET_teste <- readr::read_rds("dados-ajustados-teste/previsao-treino-prophet-boaforma.Rda") %>%
  dplyr::rename(ATUAL = REAL, PROPHET = PREVISAO_PROPHET)

#Carregando os pesos do modelo ENSEMBLE
pesos <- readr::read_rds("previsao-modelos-ensemble/pesos_ensemble_boaforma.Rda")

tbl_teste <- STLF_teste %>% 
  left_join(PROPHET_teste, by = c(".index", "ATUAL")) %>% 
  left_join(SARIMA_teste, by = c(".index", "ATUAL")) %>% 
  dplyr::mutate(TIPO = "In_sample",
                ENSEMBLE = pesos$peso1*STLF + pesos$peso2*PROPHET + pesos$peso3*SARIMA) %>% 
  dplyr::select(.index, ATUAL, STLF, PROPHET, SARIMA, ENSEMBLE, TIPO)

################################################################################
tbl_previsao <- readr::read_rds("previsao-modelos-ensemble/modelo_ensemble_boaforma.Rda") %>% 
  dplyr::mutate(
    .index = lubridate::ymd(.index),
    TIPO = 'Out_sample',
    ATUAL = as.numeric("NA")
  ) %>% 
  dplyr::rename(ENSEMBLE = modelo_ensemble,
                STLF = PREVISAO_STLF,
                PROPHET = PREVISAO_PROPHET,
                SARIMA = PREVISAO_SARIMA) %>% 
  dplyr::select(.index, ATUAL, STLF, PROPHET, SARIMA,ENSEMBLE, TIPO)

tbl_teste_previsao <- tbl_teste %>% 
  dplyr::full_join(tbl_previsao, 
                   by = c(".index", "ATUAL", "STLF", "PROPHET", "SARIMA",
                          "ENSEMBLE","TIPO")
  )


tbl_dados_reais <- readr::read_rds("previsao-sessoes-sites/tbl_previsao_boaforma.Rda") %>% 
  dplyr::filter(.key == "actual") %>% 
  dplyr::select(.index, ACTUAL) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(ATUAL = ACTUAL) %>% 
  dplyr::mutate(.index = lubridate::ymd(.index),
                TIPO = 'In_sample')

tbl_teste_previsao %>% 
  dplyr::full_join(tbl_dados_reais, by = c(".index","ATUAL","TIPO")) %>% 
  dplyr::arrange(.index) %>% 
  readr::write_rds(
    "tabela_previsao_consolidada/tbl_previsao_consolidada_boaforma.Rda")

rm(list=ls())