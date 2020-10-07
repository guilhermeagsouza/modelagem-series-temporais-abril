library(readr)
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(glmnet)
library(tidyverse)
library(tidyquant)
library(timetk) # Use >= 0.
###############################################################################

df_consolidados <- readxl::read_xlsx("dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")

data_iniciosemana_anosemana <- df_consolidados %>% 
  dplyr::filter(!ano %in% c(2015, 2016) & dia_da_semana == "seg") %>% 
  dplyr::select(date, isoYearIsoWeek) %>% 
  dplyr::distinct() %>% 
  arrange(date)
#Captando as datas das segundas-feiras de cada semana do ano
data_iniciosemana_anosemana

revistaveja_tbl <- df_consolidados %>%
  dplyr::filter(site == "Veja") %>% 
  dplyr::group_by(isoYearIsoWeek) %>% 
  dplyr::summarise(sessions = sum(sessions)) %>% 
  dplyr::left_join(data_iniciosemana_anosemana,
                   by = "isoYearIsoWeek") %>% 
  stats::na.omit() %>% 
  dplyr::select(date, sessions) %>% 
  rename(value = sessions) %>% 
  mutate(date = lubridate::ymd(date))

###############################################################################
revistaveja_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-02-24")),
            xmax = as.numeric(ymd("2020-09-19")),
            ymin = 0, ymax = 30000000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2018-06-02"), y =  27018949,
           color = palette_light()[[1]], label = "Região de treino") +
  annotate("text", x = ymd("2020-06-16"), y = 6000000,
           color = palette_light()[[1]], label = "Região de teste") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Gráfico de Sessões do site Veja.com", x = "") +
  theme_tq() +
  ggplot2::scale_y_continuous(labels = scales::comma)

################################################################################

# Split into training and test sets
train_tbl <- revistaveja_tbl %>% filter(date < ymd("2020-02-24"))
test_tbl  <- revistaveja_tbl %>% filter(date >= ymd("2012-02-24"))

train_tbl

################################################################################
# R E C I P E S
recipe_spec_timeseries <- recipe(value ~ ., data = train_tbl) %>%
  step_timeseries_signature(date) 

bake(prep(recipe_spec_timeseries), new_data = train_tbl)

recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(date) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_interact(~ date_month.lbl * date_day) %>%
  step_interact(~ date_month.lbl * date_mweek) %>%
  step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

# ESPECIFICAÇÃO DO MODELO
model_spec_glmnet <- linear_reg(mode = "regression", penalty = 10, mixture = 0.7) %>%
  set_engine("glmnet")

# workflow

workflow_glmnet <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)
workflow_glmnet

# TREINO
workflow_trained <- workflow_glmnet %>% fit(data = train_tbl)


prediction_tbl <- workflow_trained %>% 
  predict(test_tbl) %>%
  bind_cols(test_tbl) 
prediction_tbl

# GRÁFICO FORA DA AMOSTRA
revistaveja_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-02-24")),
            xmax = as.numeric(ymd("2020-09-19")),
            ymin = 0, ymax = 30000000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2018-06-02"), y =  27018949,
           color = palette_light()[[1]], label = "Região de treino") +
  annotate("text", x = ymd("2020-06-16"), y = 6000000,
           color = palette_light()[[1]], label = "Região de teste") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Gráfico de Sessões do site Veja", x = "") +
  theme_tq() +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  # Add predictions
  geom_point(aes(x = date, y = .pred), data = prediction_tbl, 
             alpha = 0.5, color = palette_light()[[2]]) +
  labs(title = "Modelo GLM: Previsão fora da amostra")


# MÉTRICAS
prediction_tbl %>% metrics(value, .pred)

prediction_tbl %>%
  ggplot(aes(x = date, y = value - .pred)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(span = 0.05, color = "red") +
  geom_smooth(span = 1.00, se = FALSE) +
  theme_tq() +
  labs(title = "GLM Model Residuals, Out-of-Sample", x = "")


# Extract revistaveja index
idx <- revistaveja_tbl %>% tk_index()
# Get time series summary from index
revistaveja_summary <- idx %>% tk_get_timeseries_summary()

#30 SEMANAS
idx_future <- idx %>% tk_make_future_timeseries(length_out = 30)
future_tbl <- tibble(date = idx_future) 
future_tbl

#Considero como variáveis regressoras o resumo de medidas estatísticas do período

future_predictions_tbl <- workflow_glmnet %>% 
  fit(data = revistaveja_tbl) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)




###############################################################################
revistaveja_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-02-24")),
            xmax = as.numeric(ymd("2020-09-19")),
            ymin = 0, ymax = 30000000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2018-06-02"), y =  27018949,
           color = palette_light()[[1]], label = "Região de treino") +
  annotate("text", x = ymd("2020-06-16"), y = 6000000,
           color = palette_light()[[1]], label = "Região de teste") +
  annotate("text", x = ymd("2020-09-21"), y = 4000000,
           #Adequar o eixo-y
           color = palette_light()[[1]], label = "Região de Previsão") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # future data
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess') + 
  labs(title = "Site Veja - Previsão 30 semanas", x = "") +
  theme_tq() +
  ggplot2::scale_y_continuous(labels = scales::comma)
###############################################################################

# ERROS DE PREVISÃO
# Calculate standard deviation of residuals
test_resid_sd <- prediction_tbl %>%
  summarize(stdev = sd(value - .pred))
future_predictions_tbl <- future_predictions_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * test_resid_sd$stdev,
    lo.80 = .pred - 1.28 * test_resid_sd$stdev,
    hi.80 = .pred + 1.28 * test_resid_sd$stdev,
    hi.95 = .pred + 1.96 * test_resid_sd$stdev
  )

# PREVISÃO COM INTERVALO DE CONFIANÇA
revistaveja_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(y = .pred, ymin = lo.95, ymax = hi.95), 
              data = future_predictions_tbl, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(y = .pred, ymin = lo.80, ymax = hi.80, fill = key), 
              data = future_predictions_tbl,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess', color = "white") + 
  labs(title = "Site Veja - Previsão de 30 semanas", x = "") +
  theme_tq()
