fun_plot_seasonal <- function(df) {
  
  highcharter::highchart() %>%
    highcharter::hc_add_series(
      df,
      name = "Peso dia da semana",
      type = "bar",
      highcharter::hcaes(x = wday_name, y = seasonal_std, color = cor),
      showInLegend = FALSE
    ) %>%
    highcharter::hc_xAxis(type = "category") %>%
    highcharter::hc_yAxis(min = -1.1, max = 1.1) #%>%
    # highcharter::hc_title(text = "Peso por dia da semana")
  
}
