# A ideia desse script é fazer uma média ponderada dos modelos de previsão.
# Começando pela comparação dos dados de teste dos modelos ETS, PROPHET E SARIMA.

tbl_guiadoestudante <- readr::read_rds("previsao-sessoes-sites/tbl_previsao_guiadoestudante.Rda")

# Um pequeno teste
y_real <- runif(n = 10, min = 0.5, max = 2)
y_ajustado_modelo1 <- runif(n = 10, min = 0.5, max = 2)
y_ajustado_modelo2 <- runif(n = 10, min = 0.5, max = 2)
df <- data.frame(y_real, y_ajustado_modelo1, y_ajustado_modelo2)


lista_df <- list()
mape <- list()

for (i in 1:100) {
  
  mape[i] <- df %>% 
    
    dplyr::mutate(y_blend = (y_ajustado_modelo1*i/100 + y_ajustado_modelo2*(100-i)/100),
                  erro_abs = abs(y_real-y_blend)/y_real/100
                  ) %>% 
    dplyr::summarise(mape = 100*mean(erro_abs))
}

#Qual menor peso
data.frame(peso = 1:100, mape = unlist(mape)) %>% 
  arrange(mape) #Suponhamos que deu 56 o peso para o primeiro modelo

# Logo teríamos uma previsão ponderada
previsao_boaforma %>% 
  mutate(previsao_ponderada = (56*y + (100-56)*Point.Forecast)/100)
  
           