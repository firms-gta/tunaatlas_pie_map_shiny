plotTotalCatchesUI <- function(id) {
  ns <- NS(id)
  dygraphOutput(ns("catches_by_year"), width = "100%", height = "400px") %>% withSpinner()
}

plotTotalCatchesServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define the namespace
    
    data_time_serie <- reactive({
      req(data())
      
      flog.info("Creating overall time series data")
      result <- data.table::setDT(data())[, .(measurement_value = sum(measurement_value, na.rm = TRUE)), by = year]
      # result <- data() %>%
      #   dplyr::group_by(year) %>%
      #   dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE)) # Handle NA values
      
      flog.info("Overall time series data: %s", head(data))
      result
    })
    
    output$catches_by_year <- renderDygraph({
      df_i1 = data_time_serie()  # Ensure this reactive expression or function returns the desired data frame
      df_i1 <- as_tibble(df_i1)  # Convert it to a tibble if not already one
      
      # Ensure 'year' column is in Date format required by xts
      df_i1$year <- as.Date(as.character(df_i1$year), format = "%Y")
      
      # Create an xts object for dygraph
      tuna_catches_timeSeries <- xts(df_i1$measurement_value, order.by = zoo::as.yearmon(df_i1$year, "%Y"))
      
      # Create the area chart
      g1 <- dygraph(tuna_catches_timeSeries) %>%
            dyOptions(fillGraph = TRUE) %>%
            dyRangeSelector()
      
      g1  # Return the dygraph object for rendering
    })
  })
}
