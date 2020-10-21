library(magrittr)
library(anomalize)
library(forecast)
library(tidyverse)
source("chart/prep_data/fun_break_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_prep_df_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_basal.R", encoding = "UTF-8")


################################################################################

# 1.0 INTRODUÇÃO - Tratamento dos dados e análise exploratória

horizonte_previsao <- 16

# Base de FERIADOS
df_feriado <- readxl::read_xlsx("dados_externos/feriados_anbima.xlsx") %>% 
  mutate(
    ano_semana = 100*lubridate::isoyear(.index) + lubridate::isoweek(.index),
    feriado = 1
  ) %>% 
  dplyr::select(ano_semana, feriado) %>% 
  dplyr::distinct()

#Lendo base de dados guiadoestudante
df_base <- readr::read_rds("dados_p_graficos/base_guiadoestudante.Rda") %>% 
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
    ano_semana = 100*lubridate::isoyear(x) + lubridate::isoweek(x)
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


#Na segunda quebra estrutural a variância explode

df_base %>% 
  plotly::plot_ly(
    x = ~x,
    y = ~y,
    type = 'scatter',
    mode = 'lines',
    color = ~quebra_estrutural
  )

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
    ano_semana = 100*lubridate::year(x) + lubridate::isoweek(x),
  ) %>% 
  #Unindo a base de feriados
  dplyr::left_join(df_feriado, by = c("ano_semana")) %>% 
  dplyr::mutate(feriado = ifelse(is.na(feriado),0,1)) %>% 
  #Unindo a base de COVID-19
  dplyr::left_join(y = df_covid, by = c("ano_semana")) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(quebra_estrutural = as.factor(fun_break_point(y)))

################################################################################
# 2.0 MODELAGEM

horizonte_previsao <- 16

treino<- df_w_outliers[1:(nrow(df_w_outliers)-horizonte_previsao),] %>% 
  dplyr::rename(ds = x)

teste <- df_w_outliers[(nrow(df_w_outliers)-horizonte_previsao+1):nrow(df_w_outliers),] %>% 
  dplyr::rename(ds = x)

serie_treino <- stats::ts(data = treino$y[1:(nrow(df_base)-horizonte_previsao)],
                          start = 2015 + 31/365.25,
                          frequency = 365.25/7
)

serie_treino %>% 
  stats::plot.ts()

# Gráfico com a ACF/PACF
forecast::ggtsdisplay(x = diff(log(serie_treino), 24))
# 2 lags significativos no PACF, sugerindo AR(2) na parte não sazonal
# 1 lag na parte sazonal

#2. Ajuste do Modelo
fit.air <- forecast::Arima(
  y = serie_treino, 
  order = c(1,0,0), 
  seasonal = c(1,0,0),
  method = "ML", #antes era ML
  lambda = 0,
  xreg = cbind(
    treino$anomaly,
    treino$feriado,
    treino$quebra_estrutural
  )
)

#Significância dos parâmetros
BETS::t_test(fit.air)

ggplot2::autoplot(fit.air) 

forecast::checkresiduals(fit.air)

#MAPE dentro da amostra
accuracy(fit.air)

#Gráfico de diagnóstico
diag <- tsdiag(fit.air, gof.lag = 20)

# P-valor =0.5453 > alfa=0.05, não rejeitamos H0 de não existência da autocorrelação serial até o lag 24.
stats::Box.test(x = fit.air$residuals, lag = 24, type = "Ljung-Box", 
                fitdf = 3)

library(FinTS)
FinTS::ArchTest(fit.air$residuals, lags = 12) #não há presença de efeito garch

require(normtest)
normtest::jb.norm.test(fit.air$residuals, nrepl = 2000) #***

shapiro.test(fit.air$residuals)

previsao <- forecast::forecast(object = fit.air, 
                               h = horizonte_previsao, 
                               xreg = cbind(
                                 teste$anomaly,
                                 teste$feriado,
                                 teste$quebra_estrutural
                               ),
                               level = 0.95
)
previsao
#CRIADO 05.10.2020
#ESCREVER OS VALORES AJUSTADOS DO SARIMA
source("funcoes_modelos/func_semanas_foradaamostra.R")
ano_mes_dia_teste <- df_w_outliers[1:(nrow(df_w_outliers)-horizonte_previsao),]$x %>% 
  dplyr::last() %>% lubridate::ymd()

previsao_guiadoestudante <- previsao %>% 
  data.frame() %>% 
  cbind(teste) %>% 
  dplyr::mutate(.index = func_semanas_foradaamostra(
    h_semanas = horizonte_previsao,
    data_inicial = ano_mes_dia_teste)
  ) %>% 
  dplyr::rename(PREVISAO_SARIMA = Point.Forecast, REAL = y) %>% 
  dplyr::select(.index, PREVISAO_SARIMA, REAL)

#Salvando na base de teste
previsao_guiadoestudante %>% 
  readr::write_rds("dados-ajustados-teste/previsao-treino-sarima-guiadoestudante.Rda")

# Métrica MAPE
previsao_guiadoestudante %>% 
  dplyr::mutate(erro = abs((REAL - PREVISAO_SARIMA)/REAL)) %>% 
  summarise(mape = mean(erro)) %>% 
  round(.,digits = 3)

################################################################################
# Gráfico com a ACF/PACF
forecast::ggtsdisplay(x = diff(log(df_w_outliers$y), 24))
# 2 lags significativos no PACF, sugerindo AR(2) na parte não sazonal
# 1 lag na parte sazonal

fit.air2 <- forecast::Arima(
  y = stats::ts(
    data = df_w_outliers$y, frequency = 365.25/7, start = 2015 + 31/365.25
  ),
  order = c(1,0,0), 
  seasonal = c(1,0,0),
  method = "ML",
  lambda = 0,
  xreg = cbind(
    df_w_outliers$anomaly,
    df_w_outliers$feriado,
    df_w_outliers$quebra_estrutural
  )
)

fit.air2
BETS::t_test(fit.air2)
accuracy(fit.air2)



previsao_foradaamostra <- forecast::forecast(
  object = fit.air2,
  h = horizonte_previsao,
  xreg = cbind(
    rep(0,horizonte_previsao), #tag_outliers
    feriados_fora_amostra$feriado, #feriado
    rep(dplyr::last(df_w_outliers$quebra_estrutural), horizonte_previsao) #quebra estrutural
  ),
  level = 0.95
)

previsao_sarima_foradaamostra <- previsao_foradaamostra %>% 
  data.frame() %>%
  cbind(.index = weeks_foradamostra) %>% 
  dplyr::mutate(
    ano_semana = 100* lubridate::isoyear(.index) + lubridate::isoweek(.index),
    PREVISAO_SARIMA = round(Point.Forecast)
  ) %>% 
  dplyr::select(.index, ano_semana,PREVISAO_SARIMA)

################################################################################
#Salvando arquivo
previsao_sarima_foradaamostra %>% 
  readr::write_rds("previsao-modelos-sarima/previsao_guiadoestudante.Rda")

rm(list=ls())
