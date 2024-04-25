plotTotalCatchesUI <- function(id) {
  ns <- NS(id)
  dygraphOutput(ns("plot_species_by_time"), width = "100%", height = "400px")
}



plotTotalCatchesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define the namespace
    
    output$plot_species_by_time <- renderDygraph({
      df_i1 = data_time_serie()  # Ensure this reactive expression or function returns the desired data frame
      df_i1 <- as_tibble(df_i1)  # Convert it to a tibble if not already one
      
      # Ensure 'year' column is in Date format required by xts
      df_i1$year <- as.Date(as.character(df_i1$year), format = "%Y")
      
      # Create an xts object for dygraph
      tuna_catches_timeSeries <- xts(x = df_i1$measurement_value, order.by = df_i1$year)
      
      # Create the area chart
      g1 <- dygraph(tuna_catches_timeSeries) %>%
            dyOptions(fillGraph = TRUE) %>%
            dyRangeSelector()
      
      return(g1)  # Return the dygraph object for rendering
    })
  })
}
