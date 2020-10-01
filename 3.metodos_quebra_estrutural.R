base_boaforma <- readr::read_rds("dados_p_graficos/base_boaforma.Rda")
df_boaforma <- readxl::read_xlsx(
  "dados_site/sites_consolidados_2015.01.01_2020.09.19.xlsx")

ano_semana <- df_boaforma %>% 
  dplyr::mutate(isoYearIsoWeek = as.numeric(isoYearIsoWeek)) %>% 
  dplyr::filter(dia_da_semana == "seg") %>% 
  group_by(isoYearIsoWeek) %>% 
  summarise(data = dplyr::first(date))

ts_boaforma <- stats::ts(data = base_boaforma$y, 
                         frequency = 365.25/7,
                         start = c(2017,01)
)

# 1o Método
# Estatística F indica quantos pontos breakpoints?
fs.boaforma <- strucchange::Fstats(ts_boaforma ~1)
plot(fs.boaforma)
strucchange::breakpoints(fs.boaforma)
lines(strucchange::breakpoints(fs.boaforma))

# 2. Método 
bp.boaforma <- strucchange::breakpoints(ts_boaforma~1)
summary(bp.boaforma)

plot(bp.boaforma)
strucchange::breakpoints(bp.boaforma)

################################################################################

# TESTE COMPLETO - RODE AQUI

a <- c(
  runif(100),
  runif(100, min = 100, max = 200),
  runif(100, min = 1000, max = 2000)
) %>%
  ts()

ts_boaforma <- a

fs.boaforma <- strucchange::Fstats(ts_boaforma ~1)
plot(fs.boaforma)
strucchange::breakpoints(fs.boaforma)
lines(strucchange::breakpoints(fs.boaforma))

#2o Método
bp.boaforma <- strucchange::breakpoints(ts_boaforma~1)
summary(bp.boaforma)

plot(bp.boaforma)
strucchange::breakpoints(bp.boaforma)

ts_boaforma %>% plot()
