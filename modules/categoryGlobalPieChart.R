categoryGlobalPieChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pie_chart"))%>% withSpinner()
}



categoryGlobalPieChartServer <- function(id, category, reactive_data, sql_query = NULL, global_topn) {
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
      req(reactive_data(), global_topn())
      
      df_raw <- reactive_data()
      N <- global_topn()
      
      # 1) calculer les totaux globaux par catégorie
      sums <- df_raw %>%
        dplyr::group_by(cat = .data[[category]]) %>%
        dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
        dplyr::arrange(desc(total))
      
      # 2) extraire les N premiers niveaux
      top_cats <- head(sums$cat, N)
      
      # 3) recoder en "Other" et regrouper
      df_raw %>%
        dplyr::mutate(
          cat2 = ifelse(.data[[category]] %in% top_cats,
                        .data[[category]],
                        "Other")
        ) %>%
        dplyr::group_by(cat2) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        ) %>%
        dplyr::rename(!!category := cat2) %>%
        dplyr::arrange(desc(measurement_value))
    })
    
    
    
    # Render the Plotly pie chart
    output$pie_chart <- renderPlotly({
      df <- data_for_chart()
      
      # 1) Grab your full named palette
      pal <- getPalette(category)
      
      # 2) Keep only the entries actually present
      pal <- pal[names(pal) %in% df[[category]]]
      
      # 3) Now replicate/reorder it so it's exactly one color per row of df:
      colors <- unname(pal[df[[category]]])
      
      plot_ly(
        df,
        labels = as.formula(paste0("~`", category, "`")),
        values = ~measurement_value,
        type   = 'pie',
        marker = list(
          colors = colors,
          line   = list(color = '#FFFFFF', width = 1)
        )
      ) %>%
        layout(
          title = sprintf('Distribution for %s', category),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
  })
}
