library(magrittr)
library(anomalize)
library(forecast)
library(tidyverse)
library(prophet)
source("chart/prep_data/fun_break_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_prep_df_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_basal.R", encoding = "UTF-8")
source("funcoes_modelos/func_semanas_foradaamostra.R", encoding = 'UTF-8')


################################################################################

# 1.0 INTRODUÇÃO - Tratamento dos dados e análise exploratória

horizonte_previsao <- 16

# Base de FERIADOS
df_feriado <- readxl::read_xlsx("dados_externos/feriados_anbima.xlsx") %>% 
  mutate(ano_semana = func_ano_semana(.index), feriado = 1) %>% 
  dplyr::select(ano_semana, feriado) %>% 
  dplyr::distinct()

#Lendo base de dados quatrorodas
df_base <- readr::read_rds("dados_p_graficos/base_quatrorodas.Rda") %>% 
  dplyr::mutate(x = lubridate::ymd(x))

#última data da série
ano_mes_dia_final <- df_base$x %>% dplyr::last() %>% lubridate::ymd()
ano_mes_dia_final

source("funcoes_modelos/func_semanas_foradaamostra.R")
weeks_foradamostra <- func_semanas_foradaamostra(
  h_semanas = horizonte_previsao,
  data_inicial = ano_mes_dia_final
)

feriados_fora_amostra <- data.frame(x = weeks_foradamostra) %>% 
  dplyr::mutate(ano_semana = 100*lubridate::isoyear(x) + lubridate::isoweek(x)) %>% 
  dplyr::left_join(y = df_feriado, by = c("ano_semana")) %>% 
  dplyr::mutate(feriado = ifelse(test = !is.na(feriado), yes = 1, no = 0))

weeks <- df_base$x

# Base de COVID-19
df_covid <- data.frame(x = weeks) %>% 
  dplyr::mutate(
    tag_covid = ifelse(weeks > "2020-03-01", 1,0),
    ano_semana = func_ano_semana(x)
  ) %>% 
  dplyr::select(ano_semana, tag_covid)

df_base <- df_base %>% 
  dplyr::mutate(quebra_estrutural = fun_break_point(y))

estatisticas_descritivas <- df_base %>% 
  dplyr::group_by(quebra_estrutural) %>% 
  dplyr::summarise(media = mean(y),
                   mediana = median(y),
                   variancia = var(y),
                   maximo = max(y))
estatisticas_descritivas

# Nesta base tem a série, outliers, quebra estrutural, feriado e tag_covid
df_w_outliers <- df_base %>%
  dplyr::select(-quebra_estrutural) %>% 
  dplyr::arrange(x) %>%
  anomalize::time_decompose(y, method = "stl") %>%
  anomalize::anomalize(remainder, method = "iqr") %>%
  dplyr::select(x, y = observed, anomaly) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    anomaly = anomaly %>%
      dplyr::recode(
        "No"  = 0,
        "Yes" = 1
      ) %>%
      as.numeric(c("Série", "Outlier")),
    ano_semana = 100*lubridate::isoyear(x) + lubridate::isoweek(x),
  ) %>% 
  #Unindo a base de feriados
  dplyr::left_join(df_feriado, by = c("ano_semana")) %>% 
  dplyr::mutate(feriado = ifelse(is.na(feriado),0,1)) %>% 
  #Unindo a base de COVID-19
  dplyr::left_join(y = df_covid, by = c("ano_semana")) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(quebra_estrutural = as.factor(fun_break_point(y)))

df_w_outliers <- df_w_outliers %>% dplyr::rename(ds = x)

################################################################################
descricao_feriados <- readxl::read_xlsx("dados_externos/feriados_anbima.xlsx") %>% 
  dplyr::mutate(ano_semana = func_ano_semana(.index)) %>% 
  dplyr::rename(x = .index) %>% 
  dplyr::select(-dia_da_semana, -x)

holidays <- df_base %>% 
  dplyr::mutate(ano_semana = func_ano_semana(x)) %>% 
  dplyr::select(x, ano_semana, y) %>% 
  dplyr::left_join(descricao_feriados, by = c("ano_semana")) %>% 
  stats::na.omit() %>% 
  dplyr::rename(holiday = feriado, ds = x) %>% 
  dplyr::mutate(lower_window = 0, upper_window = 1) %>% 
  dplyr::select(-ano_semana,-y)

outliers <- df_w_outliers %>% 
  dplyr::filter(anomaly == 1)

#Dividindo em treino
treino <- df_w_outliers[1:(nrow(df_w_outliers)-horizonte_previsao),]
teste <- df_w_outliers[(nrow(df_w_outliers)-(horizonte_previsao-1)):nrow(df_w_outliers),]

#COM FERIADO
m <- prophet::prophet()
m <- prophet::add_regressor(m,name = 'anomaly')
m <- prophet::add_regressor(m, name = 'tag_covid')
m <- prophet::add_regressor(m, name ='feriado')
#PROPHET já capta changepoints pontos de quebra
m <- prophet::prophet(df = treino, weekly.seasonality = 'auto', holidays = holidays)

future <- prophet::make_future_dataframe(m = m, periods = horizonte_previsao, freq = 'weeks')
previsao_no_teste <- predict(m, future = future, df = teste %>% select(ds, y))

previsao_prophet_teste <- previsao_no_teste %>% dplyr::select(ds, yhat)

previsao_teste_prophet <- teste %>% dplyr::select(ds, y) %>% 
  dplyr::left_join(previsao_prophet_teste, by = c("ds")) %>% 
  dplyr::rename(.index = ds, PREVISAO_PROPHET = yhat, REAL = y) %>% 
  dplyr::mutate(.index = lubridate::ymd(.index))

previsao_teste_prophet %>% 
  dplyr::mutate(erro_absoluto = abs(REAL - PREVISAO_PROPHET)/REAL) %>% 
  summarise(mape = mean(erro_absoluto))

previsao_teste_prophet %>% 
  readr::write_rds("dados-ajustados-teste/previsao-treino-prophet-quatrorodas.Rda")
################################################################################

#SEM VARIAVEL REGRESSORA
rm(m)
m <- prophet::prophet(df = treino, weekly.seasonality = "auto")

future <- make_future_dataframe(m = m, periods = horizonte_previsao, freq = 'weeks')
previsao_no_teste <- predict(m, future = horizonte_previsao, df = teste %>% dplyr::select(ds, y))

previsao_prophet_teste <- previsao_no_teste %>% dplyr::select(ds, yhat)
previsao_prophet_teste

teste %>% 
  dplyr::select(ds, y) %>% 
  dplyr::left_join(previsao_prophet_teste, by = c("ds")) %>% 
  dplyr::mutate(erro_absoluto = abs(y-yhat)/y) %>% 
  summarise(mape = mean(erro_absoluto))

# MODELO FORA DA AMOSTRA
ultima_data <- df_w_outliers %>% 
  dplyr::select(ds) %>% dplyr::summarise(ultima_data = last(ds))

ano_semana_foradaamostra <- func_semanas_foradaamostra(
  data_inicial = ultima_data$ultima_data,
  h = horizonte_previsao
)

df_complementar <- data.frame(
  ds = ano_semana_foradaamostra
)

df_feriado_completo <- readxl::read_xlsx("dados_externos/feriados_anbima.xlsx") %>% 
  dplyr::mutate(ano_semana = func_ano_semana(.index)) %>% 
  dplyr::select(-.index,-dia_da_semana)

holidays_completo <- df_w_outliers %>% 
  dplyr::select(ds) %>% 
  rbind(df_complementar) %>% 
  dplyr::mutate(ano_semana = func_ano_semana(ds)) %>% 
  dplyr::left_join(df_feriado_completo, by = c("ano_semana")) %>% 
  stats::na.omit() %>% 
  dplyr::select(-ano_semana) %>% 
  dplyr::mutate(lower_window = 0, upper_window = 1) %>% 
  dplyr::rename(holiday = feriado)

#PREVISÃO
m_foradaamostra <- prophet::prophet(
  df = df_w_outliers, 
  weekly.seasonality = "auto", 
  holidays = holidays_completo
)

future_foradaamostra <- prophet::make_future_dataframe(
  m = m_foradaamostra,
  periods = horizonte_previsao, 
  freq = 'weeks'
)

m_foradaamostra <- prophet(
  df_w_outliers, 
  holidays = holidays_completo
)

previsao_prophet <- predict(m_foradaamostra, future_foradaamostra)

#Salvando previsão
previsao_prophet %>% 
  tail(x = ., n = horizonte_previsao) %>% 
  dplyr::rename(.index = ds, PREVISAO_PROPHET = yhat) %>% 
  dplyr::select(.index, PREVISAO_PROPHET) %>% 
  dplyr::mutate(ano_semana = func_ano_semana(.index)) %>% 
  write_rds("previsao-modelos-prophet/previsao_quatrorodas.Rda")


################################################################################

rm(list=ls())

#issues mencionando os regressores no pacote prophet
#https://github.com/facebook/prophet/issues/432
#https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html#modeling-holidays-and-special-events