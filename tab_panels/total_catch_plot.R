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
catches_by_variable_moduleUI <- function(id, variable_choices) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variable_ui")),
    plotOutput(ns("plot_year")) %>% withSpinner(),
    plotOutput(ns("plot_month")) %>% withSpinner(),
    sliderInput(
      ns("topn"),
      label = "Number of this variable to display",
      min = 0,
      max = 10,
      value = 5
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
# Module Server
catches_by_variable_moduleServer <- function(id, data_without_geom) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flog.info("[%s] Initialising catches_by_variable module", id)
    
    variable_choices <- reactive({
      flog.info("[%s] Computing available variable choices", id)
      
      df <- data_without_geom()
      
      choices <- intersect(colnames(df), variable_to_display)
      
      flog.info(
        "[%s] Available variables: %s",
        id,
        paste(choices, collapse = ", ")
      )
      
      choices
    })
    
    # Dynamically generate the selectInput based on available columns
    output$variable_ui <- renderUI({
      choices <- variable_choices()
      
      flog.info(
        "[%s] Rendering variable selector with %s choices",
        id,
        length(choices)
      )
      
      selectInput(
        ns("variable"),
        "Variable to display",
        choices = setNames(
          choices,
          gsub("_", " ", choices)
        )
      )
    })
    
    # Reactive expression for yearly data
    data_year <- reactive({
      req(input$variable)
      req(input$topn)
      
      flog.info(
        "[%s] Computing yearly data: variable=%s, topn=%s",
        id,
        input$variable,
        input$topn
      )
      
      input_data <- data_without_geom()
      
      flog.info(
        "[%s] Yearly input data: %s rows, %s columns",
        id,
        nrow(input_data),
        ncol(input_data)
      )
      
      # Get initial summarised data
      df <- input_data %>%
        dplyr::mutate(year = round(year)) %>%
        dplyr::group_by(.data[[input$variable]], year) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        ) %>%
        dplyr::ungroup()
      
      flog.info(
        "[%s] Yearly summarised data: %s rows",
        id,
        nrow(df)
      )
      
      # Determine top n groups
      top_n_groups <- df %>%
        dplyr::group_by(.data[[input$variable]]) %>%
        dplyr::summarise(
          total = sum(measurement_value),
          .groups = "drop"
        ) %>%
        dplyr::top_n(input$topn, total) %>%
        dplyr::pull(.data[[input$variable]])
      
      flog.info(
        "[%s] Yearly top groups for %s: %s",
        id,
        input$variable,
        paste(top_n_groups, collapse = ", ")
      )
      
      # Modify the dataset to group non-top n values
      df <- df %>%
        dplyr::mutate(
          !!rlang::sym(input$variable) := dplyr::if_else(
            .data[[input$variable]] %in% top_n_groups,
            as.character(.data[[input$variable]]),
            "Other"
          )
        ) %>%
        dplyr::group_by(.data[[input$variable]], year) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        )
      
      flog.info(
        "[%s] Final yearly data ready: %s rows",
        id,
        nrow(df)
      )
      
      df
    })
    
    data_month <- reactive({
      req("month" %in% names(data_without_geom()))
      req(input$variable)
      req(input$topn)
      
      flog.info(
        "[%s] Computing monthly data: variable=%s, topn=%s",
        id,
        input$variable,
        input$topn
      )
      
      input_data <- data_without_geom()
      
      flog.info(
        "[%s] Monthly input data: %s rows, %s columns",
        id,
        nrow(input_data),
        ncol(input_data)
      )
      
      # Get initial summarised data
      df <- input_data %>%
        dplyr::group_by(.data[[input$variable]], month) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        )
      
      flog.info(
        "[%s] Monthly summarised data: %s rows",
        id,
        nrow(df)
      )
      
      # Determine top n groups
      top_n_groups <- df %>%
        dplyr::group_by(.data[[input$variable]]) %>%
        dplyr::summarise(
          total = sum(measurement_value),
          .groups = "drop"
        ) %>%
        dplyr::top_n(input$topn, total) %>%
        dplyr::pull(.data[[input$variable]])
      
      flog.info(
        "[%s] Monthly top groups for %s: %s",
        id,
        input$variable,
        paste(top_n_groups, collapse = ", ")
      )
      
      # Modify the dataset to group non-top n values
      df <- df %>%
        dplyr::mutate(
          !!rlang::sym(input$variable) := dplyr::if_else(
            .data[[input$variable]] %in% top_n_groups,
            as.character(.data[[input$variable]]),
            "Other"
          )
        ) %>%
        dplyr::group_by(.data[[input$variable]], month) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        )
      
      flog.info(
        "[%s] Final monthly data ready: %s rows",
        id,
        nrow(df)
      )
      
      df
    })
    
    # Plot for monthly data
    output$plot_month <- renderPlot({
      flog.info("[%s] Rendering monthly plot", id)
      
      if (all(data_without_geom()$month == 1, na.rm = TRUE)) {
        flog.info(
          "[%s] Monthly plot skipped because all observations are assigned to month 1",
          id
        )
        return(NULL)
      }
      
      df <- data_month()
      
      df_clean <- df %>%
        dplyr::mutate(
          measurement_value = ifelse(
            is.na(measurement_value),
            0,
            measurement_value
          )
        )
      
      flog.info(
        "[%s] Monthly plot data ready: %s rows",
        id,
        nrow(df_clean)
      )
      
      p <- ggplot2::ggplot(
        df_clean,
        ggplot2::aes_string(
          x = "month",
          y = "measurement_value",
          group = input$variable,
          fill = input$variable
        )
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          position = "dodge"
        ) +
        ggplot2::labs(
          title = "Monthly data",
          x = "Month",
          y = "Measurement value"
        ) +
        ggplot2::scale_x_continuous(
          breaks = scales::pretty_breaks(n = 12),
          limits = c(1, 12)
        )
      
      flog.info("[%s] Monthly plot rendered", id)
      
      p
    })
    
    # Plot for yearly data
    output$plot_year <- renderPlot({
      flog.info("[%s] Rendering yearly plot", id)
      
      df <- data_year()
      
      df_clean <- df %>%
        dplyr::mutate(
          measurement_value = ifelse(
            is.na(measurement_value),
            0,
            measurement_value
          )
        )
      
      flog.info(
        "[%s] Yearly plot data ready: %s rows, years %s-%s",
        id,
        nrow(df_clean),
        min(df_clean$year),
        max(df_clean$year)
      )
      
      x_breaks <- seq(
        min(df_clean$year),
        max(df_clean$year),
        by = 10
      )
      
      p <- ggplot2::ggplot(
        df_clean,
        ggplot2::aes_string(
          x = "year",
          y = "measurement_value",
          group = input$variable,
          color = input$variable
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "Yearly Data",
          x = "Year",
          y = "Measurement Value"
        ) +
        ggplot2::scale_x_continuous(breaks = x_breaks)
      
      flog.info("[%s] Yearly plot rendered", id)
      
      p
    })
  })
}

