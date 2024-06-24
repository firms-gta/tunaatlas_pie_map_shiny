TimeSeriesbyDimensionUI <- function(id){
  ns <- NS(id)
  tagList(
    dygraphOutput(ns("plot_by_time_by_dim"), height = "300px")%>% withSpinner() 
  )
}
  
  
TimeSeriesbyDimensionServer <- function(id, category_var, data) {
  moduleServer(id, function(input, output, session) {
  
  
  data_time_series <- reactive({
    flog.info("Generating time series data for category: %s", category_var)
    req(data()) 
 
    df <- data() %>%
      dplyr::group_by(!!sym(category_var), year) %>%
      dplyr::summarise(measurement_value = sum(measurement_value))
    flog.info("Time series data: %s", head(df))
    df
  })
  

output$plot_by_time_by_dim <- renderDygraph({
  flog.info("Rendering time series plot")
  df <- data_time_series()
  
  palette <- getPalette(category_var)
  palette <- palette[names(palette) %in% unique(df[[category_var]])]
  flog.info("Palette used for pie chart: %s", paste(names(palette), collapse = ", "))
  
  df_wide <- df %>%
    tidyr::spread(key = !!sym(category_var), value = measurement_value, fill = 0)
  
  tuna_catches_timeSeries <- xts(df_wide[-1], order.by = as.Date(paste0(df_wide$year, "-01-01")))
  
  series_names <- colnames(tuna_catches_timeSeries)
  series_colors <- palette[series_names]
  
  g1 <- dygraph(tuna_catches_timeSeries) %>%
    dyOptions(fillGraph = TRUE, colors = series_colors) %>%
    dyGroup(colnames(tuna_catches_timeSeries)) %>%
    dyRangeSelector()
  
  flog.info("Time series plot rendered")
  g1
})
  })
}
