library(anomalize)
library(forecast)
library(tidyverse)
source("chart/prep_data/fun_break_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_prep_df_point.R", encoding = "UTF-8")
source("chart/prep_data/fun_basal.R", encoding = "UTF-8")


################################################################################
# GERAÇÃO PARA CADA REVISTA 
# Não foi possível fazer um processo de iteração porque algumas revistas
#apresentam períodos faltantes

# B O A   F O R M A 

df_base <- readr::read_rds("dados_p_graficos/base_boaforma.Rda")

horizonte_previsao <- 30

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


################################################################################
horizonte_previsao <- 30

treino<- df_base[1:(nrow(df_base)-horizonte_previsao),] %>% 
  dplyr::rename(ds = x)

teste <- df_base[(nrow(df_base)-horizonte_previsao+1):nrow(df_base),] %>% 
  dplyr::rename(ds = x)

serie_treino <- stats::ts(data = treino$y[1:(nrow(df_base)-horizonte_previsao)],
                          start = 2017 + 31/365.25,
                          frequency = 365.25/7
)

serie_treino %>% 
  stats::plot.ts()

#2. Ajuste do Modelo
fit.air <- forecast::Arima(
  serie_treino, 
  order = c(1,1,1), seasonal = c(0,0,1),
  method = "ML",
  lambda = 0
  #xreg = eventos_treino
)

#Significância dos parâmetros
BETS::t_test(fit.air)

#Gráfico de diagnóstico
diag <- tsdiag(fit.air, gof.lag = 20)

# P-valor =0.5453 > alfa=0.05, não rejeitamos H0 de não existência da autocorrelação serial até o lag 24.
stats::Box.test(x = fit.air$residuals, lag = 24, type = "Ljung-Box", 
                fitdf = 3)

library(FinTS)
FinTS::ArchTest(fit.air$residuals, lags = 12) #não há presença de efeito garch

require(normtest)
normtest::jb.norm.test(fit.air$residuals, nrepl = 2000) #***

previsao <- forecast::forecast(object = fit.air, h = 30, level = 0.95)
#CRIADO 05.10.2020
#ESCREVER OS VALORES AJUSTADOS DO SARIMA
data.frame(previsao_sarima = previsao$mean %>% as.numeric()) %>% 
readr::write_rds("previsao-ajustados-sarima/boaforma.Rda")

previsao_boaforma <- previsao %>% 
  data.frame() %>% 
  cbind(teste)

# Métrica MAPE
previsao_boaforma %>% 
  dplyr::mutate(erro = abs((y - Point.Forecast)/y)) %>% 
  summarise(mape = mean(erro)) %>% 
  round(.,3) %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_sarima_boaforma.Rda")

################################################################################
fit.air2 <- forecast::Arima(
  stats::ts(data = df_base$y, frequency = 365.25/7, start = 2017 + 31/365.25),
  order = c(1,1,1), seasonal = c(0,0,1),
  method = "ML",
  lambda = 0
)
fit.air2
BETS::t_test(fit.air2)

previsao_foradaamostra <- forecast::forecast(object = fit.air2,
                                             h = 30, 
                                             level = 0.95
)
previsao_foradaamostra

################################################################################
# PARTE FINAL

# Previsão
# Data dos períodos previstos
data_previsao <- readr::read_rds("previsao-sessoes-sites/tbl_previsao_boaforma.Rda") %>%
  dplyr::filter(.key == "prediction") %>% 
  dplyr::select(.index) %>% 
  dplyr::distinct()

previsao_sarima <- previsao_foradaamostra %>% 
  data.frame() %>% 
  dplyr::select(Point.Forecast)

#SALVANDO O MODELO SARIMA
data.frame(.index = data_previsao, 
           SARIMA = previsao_sarima$Point.Forecast) %>% 
  dplyr::mutate(isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_sarima_boaforma.Rda")

#Salvando a base de previsão atualizada com os modelos ETS, Prophet e SARIMA.
readr::read_rds("previsao-sessoes-sites/tbl_previsao_boaforma.Rda") %>% 
  dplyr::left_join(data.frame(.index = data_previsao, 
           SARIMA = previsao_sarima$Point.Forecast),
           by = ".index"
  ) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_boaforma.Rda")
