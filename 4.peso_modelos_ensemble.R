# A ideia desse script é fazer uma média ponderada dos modelos de previsão.
# Começando pela comparação dos dados de teste dos modelos ETS, PROPHET E SARIMA.

# Um pequeno teste
set.seed(0)
y_real <- runif(n = 10, min = 0.5, max = 2)
y_ajustado_modelo1 <- runif(n = 10, min = 0.5, max = 2)
y_ajustado_modelo2 <- runif(n = 10, min = 0.5, max = 2)
y_ajustado_modelo3 <- runif(n = 10, min = 0.5, max = 2)

df_previsoes <- data.frame(y_real, y_ajustado_modelo1, y_ajustado_modelo2,y_ajustado_modelo3)




fun_ensemble <- function(df, peso1,peso2,peso3) {

  mape <- list()
  
  peso1 <- peso2 <- peso3 <- seq(from = 0, to = 1, by = 0.01)
  df_peso <- tidyr::expand_grid(peso1 = peso1, peso2 = peso2, peso3 = peso3)
  df_peso <- df_peso %>% dplyr::filter(peso1 + peso2 + peso3 == 1)
  
  for (i in 1:100) {
    mape[i] <- df %>%
      
      dplyr::mutate(y_blend = (y_ajustado_modelo1*i/100 + y_ajustado_modelo2*(100-i)/100),
      erro_abs = abs(y_real-y_blend)/y_real/10
      ) %>%
      dplyr::summarise(mape = 100*mean(erro_abs))
  }
}

                                                                                                                                                                                                                                                   }
  

#Qual menor peso
data.frame(peso = 1:100, mape = unlist(mape)) %>% 
  dplyr::arrange(mape) %>% #Suponhamos que deu 56 o peso para o primeiro modelo %>% 
  dplyr::first()

# Logo teríamos uma previsão ponderada
previsao_boaforma %>% 
  mutate(previsao_ponderada = (56*y + (100-56)*Point.Forecast)/100)
  
           