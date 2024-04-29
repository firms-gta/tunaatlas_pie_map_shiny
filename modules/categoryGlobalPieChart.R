categoryGlobalPieChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pie_chart"))
}



categoryGlobalPieChartServer <- function(id, category, sql_query) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to fetch and prepare data
    data_for_chart <- reactive({
      query <- paste0(sprintf(
        "SELECT %s, sum(measurement_value) AS measurement_value FROM(", category),sql_query(),
        sprintf(") AS foo GROUP BY %s ORDER BY %s", category, category))
      df <- st_read(pool, query = query)
      df
    })
    
    # Render the Plotly pie chart
    output$pie_chart <- renderPlotly({
      df <- data_for_chart()
      
      palette <- getPalette(category)
      palette <- palette[names(palette) %in% unique(df[[category]])] # a faire mieux car par reactif 
      
      plot_ly(df, labels = as.formula(paste0("~`", category, "`")), values = ~measurement_value, type = 'pie',
              marker = list(colors = palette, line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = sprintf('Distribution for %s', category),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
}
