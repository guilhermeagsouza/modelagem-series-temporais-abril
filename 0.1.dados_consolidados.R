library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
################################################################################
boaforma <- readxl::read_xlsx("dados_site/boaforma_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Boa Forma")

capricho <- readxl::read_xlsx("dados_site/capricho_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Capricho")

claudia <- readxl::read_xlsx("dados_site/claudia_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Claudia")

guiadoestudante <- readxl::read_xlsx("dados_site/guiadoestudante_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Guia do Estudante")

quatrorodas <- readxl::read_xlsx("dados_site/quatrorodas_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Quatro Rodas")

saude <- readxl::read_xlsx("dados_site/saude_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Saúde")

superinteressante <- readxl::read_xlsx("dados_site/superinteressante_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Superinteressante")

veja <- readxl::read_xlsx("dados_site/veja_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Veja")

vocesa <- readxl::read_xlsx("dados_site/vocesa_2015.01.01_2020.09.19.xlsx") %>% 
  dplyr::mutate(site = "Você S/A")

################################################################################

df_consolidada <- boaforma %>% 
  rbind(
    capricho,
    claudia,
    guiadoestudante,
    quatrorodas,
    saude,
    superinteressante,
    veja,
    vocesa
  ) %>% 
#Crio variáveis de tempo
  dplyr::mutate(
    ano = as.numeric(substring(text = date, first = 1, last = 4)),
    mes = as.numeric(substring(text = date, first = 5, last = 6)),
    dia = as.numeric(substring(text = date, first = 7, last = 8)),
    date = lubridate::as_date(stringr::str_c(ano,mes,dia,sep = "-")),
    dia_da_semana = lubridate::wday(date, label = T)
  ) 

writexl::write_xlsx(x = df_consolidada,
              path = "dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")
################################################################################
rm(list=ls())
