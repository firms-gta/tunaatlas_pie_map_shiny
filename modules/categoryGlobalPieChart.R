categoryGlobalPieChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pie_chart"))%>% withSpinner()
}



categoryGlobalPieChartServer <- function(id, category, reactive_data, sql_query = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if(!is.null(sql_query)){
      data_for_chart <- reactive({
        query <- paste0(sprintf(
          "SELECT %s, sum(measurement_value) AS measurement_value FROM(", category),sql_query(),
          sprintf(") AS foo GROUP BY %s ORDER BY %s", category, category))
        df <- st_read(pool, query = query)
        df
      })
    }
    
    # Reactive expression to fetch and prepare data
    data_for_chart <- reactive({
      req(reactive_data())
      flog.info("Preparing data for pie chart for category: %s", category)
      df <- reactive_data() %>%
        dplyr::group_by(!!sym(category)) %>%
        dplyr::summarise(measurement_value = sum(measurement_value)) %>%
        dplyr::arrange(!!sym(category))
      flog.info("Data for pie chart: %s", head(df))
      df
    })
    
    
    
    # Render the Plotly pie chart
    output$pie_chart <- renderPlotly({
      flog.info("Rendering pie chart for category: %s", category)
      df <- data_for_chart()
      flog.info("Data used for pie chart: %s", head(df))
      
      palette <- getPalette(category)
      palette <- palette[names(palette) %in% unique(df[[category]])]
      # flog.info("Palette used for pie chart: %s", paste(names(palette), collapse = ", "))
      
      plot_ly(df, labels = as.formula(paste0("~`", category, "`")), values = ~measurement_value, type = 'pie',
              marker = list(colors = palette, line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = sprintf('Distribution for %s', category),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
}
