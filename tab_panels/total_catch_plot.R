#' UI for Catch Data by Variable Module
#'
#' This function creates a UI module for displaying the top N categories within a selected variable through dynamic plots. It includes controls for selecting the variable and specifying the number of top categories to display.
#' 
#' @param id Character. A namespace identifier for the module's UI components.
#'
#' @return A \code{tagList} object containing a slider input for selecting the number of top categories, a select input for choosing the variable, and plot outputs for yearly and monthly aggregated data.
#' 
#' @importFrom shiny NS tagList sliderInput selectInput plotOutput
#' @export

# Module UI
catches_by_variable_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(selectInput(ns("variable"), 
                "Variable to Display", 
                choices = c("Species" = "species", 
                            "Fishing Fleet" = "fishing_fleet", 
                            "Gear Type" = "gear_type",
          "Fishing mode" = "fishing_mode")),
plotOutput(ns("plot_year")),    plotOutput(ns("plot_month")), 
    sliderInput(ns("topn"),
                label = "Number of this variable to display",
                min = 0,
                max = 10,
                value = 5,
    )
  )
}


#' Server Logic for Catch Data by Variable Module
#'
#' This server module processes the selected variable and top N categories to visualize yearly and monthly data aggregations. It generates two plots: one for yearly data and one for monthly data, based on the top N categories of the selected variable.
#'
#' @param id Character. The namespace identifier for the UI components, ensuring isolation within the app.
#' @param data_without_geom A reactive expression or a function returning a dataframe. This dataframe should contain the columns necessary for the visualization, excluding any geometric information.
#'
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr group_by summarise mutate top_n pull ungroup
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_line labs
#' @export

# Module Server
catches_by_variable_moduleServer <- function(id, data_without_geom) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression for yearly data
    data_year <- reactive({
      req(input$variable)  # Ensure that a variable is selected
      req(input$topn)      # Ensure that the number for top n is provided
      
      # Get initial summarised data
      df <- data_without_geom() %>%
        dplyr::group_by(.data[[input$variable]], year) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>% ungroup()
      
      # Determine top n groups
      top_n_groups <- df %>%
        dplyr::group_by(.data[[input$variable]]) %>%
        dplyr::summarise(total = sum(measurement_value)) %>%
        dplyr::top_n(input$topn, total) %>%
        pull(.data[[input$variable]])
      
      # Modify the dataset to group non-top n values
      df <- df %>%
        dplyr::mutate(!!sym(input$variable) :=if_else(.data[[input$variable]] %in% top_n_groups, as.character(.data[[input$variable]]), "Other")) %>%
        dplyr::group_by(.data[[input$variable]], year) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      df
    })
    data_month <- reactive({
      req(input$variable)  # Ensure that a variable is selected
      req(input$topn)      # Ensure that the number for top n is provided
      
      # Get initial summarised data
      df <- data_without_geom() %>%
        dplyr::rename(month = year) %>% 
        dplyr::group_by(.data[[input$variable]], month) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      # Determine top n groups
      top_n_groups <- df %>%
        dplyr::group_by(.data[[input$variable]]) %>%
        dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
        dplyr::top_n(input$topn, total) %>%
        pull(.data[[input$variable]])
      
      # Modify the dataset to group non-top n values
      df <- df %>%
        dplyr::mutate(grouped_variable = if_else(.data[[input$variable]] %in% top_n_groups, as.character(.data[[input$variable]]), "Other")) %>%
        dplyr::group_by(grouped_variable, month) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      df
    })
    
    
    # Plot for monthly data
    output$plot_month <- renderPlot({
      df <- data_month()  # Get the reactive monthly data
      
     p <- ggplot(df, aes_string(x = "year", y = "measurement_value", group = input$variable, fill = input$variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Monthly Data", x = "Month", y = "Measurement Value")
      
      
      p
    })
    
    # Plot for yearly data
    output$plot_year <- renderPlot({
      df <- data_year()  # Get the reactive yearly data
      
      p <- ggplot(df, aes_string(x = "year", y = "measurement_value", group = input$variable, color = input$variable)) +
        geom_line() + labs(title = "Yearly Data", x = "Year", y = "Measurement Value")
      
      
      p
    })
  })
}

