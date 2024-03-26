# Module UI
catches_by_variable_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(sliderInput(ns("topn"),
    label = "Top number of : variable to display (on the total selected data)",
    min = 0,
    max = 10,
    value = 5,
  ),
    selectInput(ns("variable"), 
                "Variable to Display", 
                choices = c("Species" = "species", 
                            "Fishing Fleet" = "fishing_fleet", 
                            "Gear Type" = "gear_type")),
    plotOutput(ns("plot_year")),    plotOutput(ns("plot_month"))
  )
}




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

