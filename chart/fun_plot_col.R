
fun_plot_col <- function(df){
  assertthat::assert_that(is.data.frame(df))
  
  df %>% 
    dplyr::mutate(
      ano = as.factor(substring(text = date, first = 1, last = 4)),
      mes = substring(text = date, first = 5, last = 6),
      dia = substring(text = date, first = 7, last = 8),
      Data = lubridate::as_date(stringr::str_c(ano,mes,dia, sep = "-")),
      dia_da_semana = forcats::fct_relevel(lubridate::wday(Data, label = T),
                                           "seg", "ter", "qua", 
                                           "qui", "sex", "sáb", 
                                           "dom")
    ) %>% 
    dplyr::group_by(ano, dia_da_semana) %>% 
    dplyr::summarise(sessions = mean(sessions)) %>% 
    dplyr::filter(ano != "2015") %>% 
    ggplot2::ggplot() +
    ggplot2::geom_col(aes(x = dia_da_semana, y = sessions, fill = ano), position = "dodge") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = '', y = '', 
                  title = 'Número médio sessões por dia da semana - Boa Forma',
                  subtitle = 'Período analisado: 2016 a 2020'
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values=c("#207DA8","#144E69","#2CADE8","#2FB6F5","#279ACF"))
  
}