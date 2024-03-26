categoryGlobalPieChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pie_chart"))
}



categoryGlobalPieChartServer <- function(id, category) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to fetch and prepare data
    data_for_chart <- reactive({
      query <- sprintf(
        "SELECT %s, sum(measurement_value) AS measurement_value FROM(", category) + 
        sql_query() + 
        sprintf(") AS foo GROUP BY %s ORDER BY %s", category, category)
      df <- st_read(pool, query = query)
      df
    })
    
    # Render the Plotly pie chart
    output$pie_chart <- renderPlotly({
      df <- data_for_chart()
      palette <- palette3[names(palette3) %in% unique(df[[category]])]
      
      plot_ly(df, labels = ~ !!sym(category), values = ~measurement_value, type = 'pie',
              marker = list(colors = palette, line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = sprintf('Distribution for %s', category),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  })
}
