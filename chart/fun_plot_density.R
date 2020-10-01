fun_plot_density <- function(df) {
  
  highcharter::highchart() %>%
    highcharter::hc_add_series(
      df %>% dplyr::filter(classe == "referência"),
      name = mes_referencia,
      type = "area",
      highcharter::hcaes(x = x, y = y),
      color = highcharter::hex_to_rgba(x = "#d3d3d3"),
      fillColor = highcharter::hex_to_rgba(x = "#d3d3d3", 0.3)
    ) %>%
    highcharter::hc_add_series(
      df %>% dplyr::filter(classe == "análise"),
      name = mes_analise,
      type = "area",
      highcharter::hcaes(x = x, y = y),
      color = highcharter::hex_to_rgba(x = "#005824"),
      fillColor = highcharter::hex_to_rgba(x = "#005824", 0.3)
    ) %>%
    highcharter::hc_xAxis(
      crosshair = TRUE
    ) %>%
    highcharter::hc_yAxis(
      crosshair = TRUE,
      gridLineWidth = 0,
      labels = list(
        enabled = FALSE
      )
    ) %>%
    highcharter::hc_tooltip(enabled = FALSE)# %>%
    # highcharter::hc_legend(
    #   align = "right",
    #   verticalAlign = "top",
    #   layout = "vertical"
    # )
  
}
