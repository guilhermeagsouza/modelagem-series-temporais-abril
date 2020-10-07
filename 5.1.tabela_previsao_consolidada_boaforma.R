#Dados com todos os modelos
tbl_teste <- readr::read_rds("dados-modelo-teste/boaforma.Rda")

#ETS teste
ETS_teste <- tbl_teste %>% 
  dplyr::select(date, Real_ETS, Previsao_ETS) %>% 
  stats::na.omit() %>% 
  dplyr::rename(.index = date, ATUAL = Real_ETS, ETS = Previsao_ETS)

#PROPHET teste
PROPHET_teste <- tbl_teste %>% 
  dplyr::select(date, Real_PROPHET, Previsao_PROPHET) %>% 
  stats::na.omit() %>% 
  dplyr::rename(.index = date, ATUAL = Real_PROPHET, PROPHET = Previsao_PROPHET)

#SARIMA teste
SARIMA_teste <- readr::read_rds("previsao-ajustados-sarima/boaforma.Rda") %>% 
  #Junto uma coluna do ETS_teste ou do PROPHET_teste
  cbind(
    data.frame(.index = ETS_teste$.index, ATUAL = ETS_teste$ATUAL)
  ) %>% 
  dplyr::rename(SARIMA = previsao_sarima) %>% 
  dplyr::select(.index, ATUAL, SARIMA)

#Carregando os pesos do modelo ENSEMBLE
pesos <- readr::read_rds("previsao-modelos-ensemble/pesos_ensemble_boaforma.Rda")

tbl_teste <- ETS_teste %>% 
  left_join(PROPHET_teste, by = c(".index", "ATUAL")) %>% 
  left_join(SARIMA_teste, by = c(".index", "ATUAL")) %>% 
  dplyr::mutate(TIPO = "In_sample",
                ENSEMBLE = pesos$peso1*ETS + pesos$peso2*PROPHET + pesos$peso3*SARIMA) %>% 
  dplyr::select(.index, ATUAL, ETS, PROPHET, SARIMA, ENSEMBLE, TIPO)

################################################################################
tbl_previsao <- readr::read_rds("previsao-modelos-ensemble/modelo_ensemble_boaforma.Rda") %>% 
  dplyr::mutate(
    .index = lubridate::ymd(.index),
    TIPO = 'Out_sample',
    ATUAL = as.numeric("NA")
  ) %>% 
  dplyr::rename(ENSEMBLE = modelo_ensemble) %>% 
  dplyr::select(.index, ATUAL, ETS, PROPHET, SARIMA,ENSEMBLE, TIPO)

tbl_teste_previsao <- tbl_teste %>% 
  dplyr::full_join(tbl_previsao, 
                   by = c(".index", "ATUAL", "ETS", "PROPHET", "SARIMA",
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

