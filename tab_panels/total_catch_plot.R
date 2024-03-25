# Module UI
catches_by_variable_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("variable"), "Variable to Display", choices = c("Species" = "species", "Fishing Fleet" = "fishing_fleet", "Gear Type" = "gear_type", "Ocean" = "ocean")),
    radioButtons(ns("viewType"), "View Type", choices = c("Continuous" = "continuous", "Discrete" = "discrete")),
    plotOutput(ns("plot"))
  )
}




# Module Server
totalCatchplotUIServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot_total_catch_by_variables <- renderPlot({
    
    data_month <- data() %>%
        dplyr::group_by(!!sym(input$variable), month) %>% 
        dplyr::summarise(measurement_value = mean(measurement_value))
    
    data_year <- data() %>%
      dplyr::group_by(!!sym(input$variable), year) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      if(input$viewType == "continuous"){
        data_month <- ggplot(data_month, aes(x = year, y = input$value, color = type)) + 
          geom_line()
        data_year <- ggplot(data_year, aes(x = year, y = input$value, color = type)) + 
          geom_line()
        
      } else {
        data_month <- ggplot(data_month, aes(x = year, y = input$value, color = type)) + 
          geom_line() + geom_bar(stat = "identity", position = "dodge")
        data_year <- ggplot(data_year, aes(x = year, y = input$value, color = type)) + 
          geom_line() + geom_bar(stat = "identity", position = "dodge")
      }
    return(data_month, data_year)
      
      plot("plot_total_catch_by_variables", main = paste("Captures ", input$viewType, "de", input$variable))
    })
  })
}
