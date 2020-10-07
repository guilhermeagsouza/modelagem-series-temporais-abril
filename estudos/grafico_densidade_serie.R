plot_density_boaforma <- readr::read_rds("dados_p_graficos/base_boaforma.Rda") %>% 
  dplyr::mutate(Ano = as.factor(lubridate::year(x))) %>% 
  ggplot2::ggplot(aes(y,  fill = Ano, colour = Ano)) +
  ggplot2::geom_density(alpha = 0.1) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(x = '', 
                y = ''
  ) +
  ggplot2::theme_bw()

plot_density_boaforma %>% 
  plotly::ggplotly() %>% 
  plotly::layout(
    #title = 'Gráfico da distribuição da variável Sessões do site Boa Forma',
    legend = list(orientation = "h",   #mostrar entradas horizontalmente
                  xanchor = "center",  #usar centralizado
                  x = 0.5) #coloca a legenda no centro do eixo-x
  )