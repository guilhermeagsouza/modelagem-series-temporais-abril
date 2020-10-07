library(modeltime)
library(readxl)
library(tidyverse)
library(timetk)
library(tidymodels)

df_consolidados <- readxl::read_xlsx(
  "dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")

data_iniciosemana_anosemana <- df_consolidados %>% 
  #**Captando as datas das segundas-feiras de cada semana do ano
  dplyr::filter(dia_da_semana == "seg") %>% 
  dplyr::select(date, isoYearIsoWeek) %>% 
  dplyr::distinct() %>% 
  arrange(date)
data_iniciosemana_anosemana


by_site <- df_consolidados %>% 
  
  dplyr::filter(
    dia_da_semana == "seg" &
      !site %in% c("Você S/A") &
      sessions != 0
  ) %>% 
  
  dplyr::group_by(site, isoYearIsoWeek) %>% 
  dplyr::summarise(sessions = sum(sessions)) %>% 
  
  #Juntando o data_iniciosemana_anosemana para pegar as segundas-feiras
  dplyr::left_join(data_iniciosemana_anosemana, by = c("isoYearIsoWeek"))
###############################################################################

# 1. SITE BOA FORMA
serie_boaforma <- readr::read_rds("dados_p_graficos/base_boaforma.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras
n_teste <- 30

splits <- rsample::initial_time_split(serie_boaforma, 
                                      prop = round(1-n_teste/nrow(serie_boaforma),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = rsample::training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  parsnip::set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  parsnip::fit(value ~ date, data = rsample::training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl


# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = rsample::testing(splits))
calibration_tbl

dados_modelo_teste <- 
plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                          rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/boaforma.Rda")

#GRÁFICO
calibration_tbl %>%
  modeltime::modeltime_forecast(
    new_data    = rsample::testing(splits),
    actual_data = serie_boaforma
  ) %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_boaforma <- calibration_tbl %>%
  modeltime::modeltime_accuracy()
metricas_boaforma

metricas_boaforma %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_boaforma.Rda")
  

# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime::modeltime_refit(data = serie_boaforma)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_boaforma)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Boa Forma",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_boaforma.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)
################################################################################
# 2. SITE CAPRICHO

serie_capricho <- readr::read_rds("dados_p_graficos/base_capricho.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_capricho, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_capricho),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

#########
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/capricho.Rda")
##########


#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_capricho
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_capricho <- calibration_tbl %>%
  modeltime_accuracy()

metricas_capricho %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_capricho.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_capricho)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_capricho)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Capricho",
                isoYearIsoWeek= 100*lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_capricho.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 3. SITE GUIA DO ESTUDANTE

serie_guiadoestudante <- readr::read_rds("dados_p_graficos/base_guiadoestudante.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_guiadoestudante, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_guiadoestudante),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/guiadoestudante.Rda")
##################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_guiadoestudante
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_guiadoestudante <- calibration_tbl %>%
  modeltime_accuracy()

metricas_guiadoestudante %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_guiadoestudante.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_guiadoestudante)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_guiadoestudante)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Guia do Estudante",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_guiadoestudante.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 4. SITE QUATRO RODAS

serie_quatrorodas <- readr::read_rds("dados_p_graficos/base_quatrorodas.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_quatrorodas, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_quatrorodas),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/quatrorodas.Rda")
################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_quatrorodas
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_quatrorodas <- calibration_tbl %>%
  modeltime_accuracy()

metricas_quatrorodas %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_quatrorodas.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_quatrorodas)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_quatrorodas)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Quatro Rodas",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_quatrorodas.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 5. SITE SAÚDE

serie_saude <- readr::read_rds("dados_p_graficos/base_saude.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_saude, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_saude),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/saude.Rda")
################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_saude
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_saude <- calibration_tbl %>%
  modeltime_accuracy()

metricas_saude %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_saude.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_saude)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_saude)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Saúde",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_saude.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 6. SITE SUPERINTERESSANTE

serie_superinteressante <- readr::read_rds("dados_p_graficos/base_superinteressante.Rda") %>% 
  dplyr::rename(date = x, value = y)

#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_superinteressante, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_superinteressante),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/superinteressante.Rda")
################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_superinteressante
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_superinteressante <- calibration_tbl %>%
  modeltime_accuracy()

metricas_superinteressante %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_superinteressante.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_superinteressante)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_superinteressante)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Superinteressante",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_superinteressante.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 7. SITE VEJA

serie_veja <- readr::read_rds("dados_p_graficos/base_veja.Rda") %>% 
  dplyr::rename(date = x, value = y)
#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_veja, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_veja),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/veja.Rda")
################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_veja
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_veja <- calibration_tbl %>%
  modeltime_accuracy()

metricas_veja %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_veja.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_veja)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_veja)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Veja",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_veja.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

################################################################################
# 8. SITE Claudia

serie_claudia <- readr::read_rds("dados_p_graficos/base_claudia.Rda") %>% 
  dplyr::rename(date = x, value = y)
#Dividindo em duas amostras

n_teste <- 30

splits <- rsample::initial_time_split(serie_claudia, 
                                      #Dividindo em 30 observações (automatizado)
                                      prop = round(1-n_teste/nrow(serie_claudia),2))
splits

# ETS
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = training(splits))


# PROPHET
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet", weekly.seasonality=TRUE) %>%
  fit(value ~ date, data = training(splits))

models_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet
)
models_tbl

# Avaliando nos dados de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

################
dados_modelo_teste <- 
  plyr::ldply(calibration_tbl$.calibration_data, fun = data.frame) %>% 
  dplyr::mutate(modelo = c(rep(calibration_tbl[1,3]$.model_desc, n_teste),
                           rep(calibration_tbl[2,3]$.model_desc, n_teste))
  ) %>% 
  tidyr::pivot_wider(names_from = modelo, values_from = c(.actual, .prediction))

colnames(dados_modelo_teste) <- c("date", "residuals", "Real_ETS","Real_PROPHET",
                                  "Previsao_ETS","Previsao_PROPHET")

dados_modelo_teste %>% 
  data.frame() %>% 
  readr::write_rds("dados-modelo-teste/claudia.Rda")
################

#GRÁFICO
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = serie_claudia
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
  )

# Métricas
metricas_claudia <- calibration_tbl %>%
  modeltime_accuracy()

metricas_claudia %>% 
  readr::write_rds(
    "metricas-previsao-sessoes-sites/metricas_previsao_claudia.Rda")


# PREVISAO PARA FORA DA AMOSTRA
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = serie_claudia)

tbl_previsao <- refit_tbl %>%
  modeltime::modeltime_forecast(h = 30, actual_data = serie_claudia)

tbl_previsao %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25
  )


# GRÁFICO DA SÉRIE DE PREVISÃO
tbl_previsao %>% 
  dplyr::left_join(data_iniciosemana_anosemana %>% 
                     dplyr::rename(.index = date),
                   by = c(".index")
  ) %>% 
  tidyr::pivot_wider(names_from = c(.model_desc), values_from = c(.value)) %>% 
  dplyr::mutate(site = "Claudia",
                isoYearIsoWeek= 100* lubridate::year(.index) + lubridate::isoweek(.index)) %>% 
  readr::write_rds("previsao-sessoes-sites/tbl_previsao_claudia.Rda")

rm(list=setdiff(ls(), 
                c("df_consolidados", "data_iniciosemana_anosemana","by_site",
                  "dados_modelo_teste"))
)

