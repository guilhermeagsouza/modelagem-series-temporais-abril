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
dados_modelo_teste <- readr::read_rds("dados-modelo-teste/claudia.Rda")

REAL <- dados_modelo_teste %>% 
  dplyr::select(date, Real_ETS) %>% 
  dplyr::rename(y_real = Real_ETS) %>% 
  stats::na.omit()

PREVISAO_ETS <- dados_modelo_teste %>% dplyr::select(date, Previsao_ETS) %>% 
  stats::na.omit() %>% 
  dplyr::rename(y_ajustado_modelo1 = Previsao_ETS)

PREVISAO_PROPHET <- dados_modelo_teste %>% dplyr::select(date, Previsao_PROPHET) %>% 
  stats::na.omit() %>% 
  dplyr::rename(y_ajustado_modelo2 = Previsao_PROPHET)

PREVISAO_SARIMA <- readr::read_rds("previsao-ajustados-sarima/claudia.Rda") %>% 
  rename(y_ajustado_modelo3 = previsao_sarima)

df_previsoes <- REAL %>% 
  dplyr::left_join(PREVISAO_ETS, by = c("date")) %>% 
  dplyr::left_join(PREVISAO_PROPHET, by = c("date")) %>% 
  cbind(PREVISAO_SARIMA)

df_ensemble <- df_previsoes %>%
  dplyr::select(-date) %>% 
  fun_ensemble()

#Selecionando apenas os pesos diferentes de 0
pesos <- df_ensemble %>% 
  dplyr::filter(peso1 !=0 & peso2 !=0 & peso3 !=0) %>% 
  slice(1)

pesos

pesos %>% write_rds("previsao-modelos-ensemble/pesos_ensemble_claudia.Rda")

################################################################################
previsao <- readr::read_rds("previsao-sessoes-sites/tbl_previsao_claudia.Rda") %>% 
  dplyr::filter(.key == "prediction")

modelo_ets <- previsao %>% select(.index, starts_with("ETS")) %>% 
  stats::na.omit() %>% 
  dplyr::distinct()
colnames(modelo_ets) <- c(".index", "ETS")

modelo_prophet <- previsao %>% select(.index, starts_with("PROPHET")) %>% 
  stats::na.omit() %>% 
  dplyr::distinct()
modelo_sarima <- previsao %>% select(.index, starts_with("SARIMA")) %>% 
  stats::na.omit() %>% 
  dplyr::distinct()

modelo_ensemble <- modelo_ets %>% 
  dplyr::left_join(modelo_prophet, by = c(".index")) %>% 
  dplyr::left_join(modelo_sarima, by = c(".index")) %>% 
  dplyr::mutate(
    modelo_ensemble = ETS*pesos$peso1 + 
      PROPHET*pesos$peso2 + 
      SARIMA*pesos$peso3,
    MAPE_ensemble = pesos$mape,
    isoYearIsoWeek = 100*lubridate::year(.index) + lubridate::isoweek(.index)
  )

modelo_ensemble %>% 
  readr::write_rds("previsao-modelos-ensemble/modelo_ensemble_claudia.Rda")

rm(list=ls())
