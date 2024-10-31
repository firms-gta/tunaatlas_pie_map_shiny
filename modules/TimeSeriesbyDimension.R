TimeSeriesbyDimensionUI <- function(id){
  ns <- NS(id)
  tagList(
    dygraphOutput(ns("plot_by_time_by_dim"), height = "300px") %>% withSpinner()
  )
}

TimeSeriesbyDimensionServer <- function(id, category_var, data) {
  moduleServer(id, function(input, output, session) {
    
    data_time_series <- reactive({
      flog.info("Generating time series data for category: %s", category_var)
      req(data())
      
      # Convert to data.table for faster processing
      dt <- as.data.table(data())
      
      # Group by the category variable and year, and then sum measurement_value
      df <- dt[, .(measurement_value = sum(measurement_value)), by = c(category_var, "year")]
      flog.info("Time series data: %s", head(df))
      df
    })
    
    output$plot_by_time_by_dim <- renderDygraph({
      flog.info("Rendering time series plot")
      df <- data_time_series()
      
      # Define color palette
      palette <- getPalette(category_var)
      palette <- palette[names(palette) %in% unique(df[[category_var]])]
      
      # Reshape data to wide format
      df_wide <- dcast(df, year ~ get(category_var), value.var = "measurement_value", fill = 0)
      
      # Convert to xts time series format
      tuna_catches_timeSeries <- xts(df_wide[, -1, with = FALSE], order.by = as.Date(paste0(df_wide$year, "-01-01")))
      
      # Apply colors to each series
      series_names <- colnames(tuna_catches_timeSeries)
      series_colors <- palette[series_names]
      
      # Render the dygraph
      g1 <- dygraph(tuna_catches_timeSeries) %>%
        dyOptions(fillGraph = TRUE, colors = series_colors) %>%
        dyGroup(series_names) %>%
        dyRangeSelector()
      
      flog.info("Time series plot rendered")
      g1
    })
  })
}
