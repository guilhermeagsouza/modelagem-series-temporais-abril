fun_plot_ts <- function(df, revista_name) {
  
  
  df_month <- df %>%
    fun_prep_df_month()
  
  df_point <- df %>%
    fun_prep_df_point()
  
  df_ma <- df %>%
    fun_prep_df_ma()
  
  df_loess <- df %>%
    fun_prep_df_loess()
  
  
  hc <- highcharter::highchart() %>%
    
    highcharter::hc_add_series(
      data = df_point,
      name = "Basal",
      type = "line",
      color = "orange",
      highcharter::hcaes(x = x, y = basal)
    ) %>%
    
    # DESATIVANDO A MÉDIA MÓVEL DE 30 DIAS PARA ESSE GRÁFICO
    #highcharter::hc_add_series(
    #  df_month,
    #  name = "Média mensal",
    #  type = "line",
    #  highcharter::hcaes(x = x_avg, y = y),
    #  marker = list(symbol = "square", radius = 4),
    #  color = "black",
    #  dashStyle = "ShortDash",
    #  step = "center",
    #  tooltip   = list(
    #    headerFormat = "",
    #    pointFormat  = "{point.name}<br><b>{point.y_tooltip}</b>"
    #  )
    #) %>%
    
    # DESATIVANDO A PARTE DE MÉDIA MENSAL
    #highcharter::hc_add_series(
    #  name = "Média mensal",
    #  type = "polygon",
    #  data = purrr::flatten(df_month$coord_polygon),
    #  color = highcharter::hex_to_rgba(x = "gray", 0.3),
    #  enableMouseTracking = FALSE,
    #  linkedTo = ":previous"
    #) %>%
    
    highcharter::hc_add_series(
      df_point,
      type = "scatter",                 
      highcharter::hcaes(x = x, y = y, group = anomaly),
      color = c(
        highcharter::hex_to_rgba(x = "#5bae93", 0.8),
        highcharter::hex_to_rgba(x = "#800080", 0.6)
      ),
      marker = list(symbol = "circle", radius = 3),
      tooltip   = list(
        headerFormat = "",
        pointFormat  = "{point.x_tooltip}<br><b>{point.y_tooltip}</b>"
      )
    ) %>%
    
    # DESATIVANDO A MÉDIA MÓVEL DE 30 DIAS PARA ESSE GRÁFICO
    #highcharter::hc_add_series(
    #  df_ma,
    #  type = "spline",                 
    #  highcharter::hcaes(x = x, y = y, group = name),
    #  color = "#005824",
    #  lineWidth = c(3)
    #) %>%
    
    highcharter::hc_xAxis(
      type     = "datetime",
      crosshair = TRUE
    ) %>%
    
    highcharter::hc_yAxis(
      crosshair = TRUE,
      gridLineWidth = 0
    ) #%>%
    
    # highcharter::hc_title(text = "Série diária de usuários") %>%
    # highcharter::hc_subtitle(text = revista_name)
  
  
  hc$x$conf_opts$lang$decimalPoint <- ","
  
  hc
  
  
}
