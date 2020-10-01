library(tidyverse)
################################################################################
#Funções
source("chart/prep_data/fun_prep_df_point.R", encoding = 'UTF-8')
source("chart/prep_data/fun_basal.R", encoding = 'UTF-8')
source("chart/prep_data/fun_prep_df_month.R", encoding = 'UTF-8')
source("chart/prep_data/fun_prep_df_ma.R", encoding = 'UTF-8')
source("chart/prep_data/fun_prep_df_loess.R", encoding = 'UTF-8')
source("chart/fun_plot_ts.R", encoding = 'UTF-8')
source("chart/prep_data/fun_calc_coord_polygon.R", encoding = 'UTF-8')

################################################################################

df_consolidados <- readxl::read_xlsx(
  "dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")

# #Captando as datas das segundas-feiras de cada semana do ano
data_iniciosemana_anosemana <- df_consolidados %>% 
  dplyr::filter(dia_da_semana == "seg") %>% 
  dplyr::select(date, isoYearIsoWeek) %>% 
  dplyr::distinct() %>% 
  arrange(date)

df_base <- df_consolidados %>% 
  dplyr::group_by(site, isoYearIsoWeek) %>% 
  dplyr::summarise(y = sum(sessions)) %>% 
  #Juntando o data_iniciosemana_anosemana para pegar as segundas-feiras
  dplyr::left_join(data_iniciosemana_anosemana, by = c("isoYearIsoWeek"))

################################################################################
# B O A   F O R M A 
#(a partir de 2017, pois as sessões antes eram iguais ou perto de zero)

df_base_boaforma <- df_base %>% 
  dplyr::filter(site == "Boa Forma" & !lubridate::year(date) %in% c(2015, 2016)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)

#df_base_boaforma %>% readr::write_rds("dados_p_graficos/base_boaforma.Rda")
################################################################################
# C A P R I C H O
df_base_capricho <- df_base %>% 
  dplyr::filter(site == "Capricho") %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_capricho %>% readr::write_rds("dados_p_graficos/base_capricho.Rda")
################################################################################
#GUIA DO ESTUDANTE
df_base_guiadoestudante <- df_base %>% 
  dplyr::filter(site == "Guia do Estudante") %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_guiadoestudante %>% readr::write_rds("dados_p_graficos/base_guiadoestudante.Rda")
################################################################################
#QUATRO RODAS
df_base_quatrorodas <- df_base %>% 
  dplyr::filter(site == "Quatro Rodas") %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_quatrorodas %>% readr::write_rds("dados_p_graficos/base_quatrorodas.Rda")
################################################################################
#SAÚDE
df_base_saude <- df_base %>% 
  dplyr::filter(site == "Saúde" & !lubridate::year(date) %in% c(2015, 2016)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_saude %>% readr::write_rds("dados_p_graficos/base_saude.Rda")
################################################################################
#SUPER INTERESSANTE
df_base_superinteressante <- df_base %>% 
  dplyr::filter(site == "Superinteressante") %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_superinteressante %>% readr::write_rds("dados_p_graficos/base_superinteressante.Rda")
################################################################################
# VEJA
df_base_veja<- df_base %>% 
  dplyr::filter(site == "Veja") %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_veja %>% readr::write_rds("dados_p_graficos/base_veja.Rda")
################################################################################
# CLAUDIA (COM PARCEIROS)
df_base_claudia <- df_base %>% 
  dplyr::filter(site == "Claudia" & !lubridate::year(date) %in% c(2015, 2016)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_claudia %>% readr::write_rds("dados_p_graficos/base_claudia.Rda")
################################################################################
# V O C Ê   S / A
#somente 36 semanas de dados de sessões
df_base_vocesa <- df_base %>%  
  dplyr::filter(site == "Você S/A" & 
                  lubridate::year(date) %in% c(2019, 2020)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  dplyr::select(date, y, -site) %>% 
  dplyr::rename(x = date)
#df_base_vocesa %>% readr::write_rds("dados_p_graficos/base_vocesa.Rda")
################################################################################