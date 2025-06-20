TimeSeriesbyDimensionUI <- function(id){
  ns <- NS(id)
  tagList(
    dygraphOutput(ns("plot_by_time_by_dim"), height = "300px") %>% withSpinner()
  )
}

TimeSeriesbyDimensionServer <- function(id, category_var, data, global_topn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_time_series <- reactive({
      flog.info("Generating time series data for category: %s", category_var)
      req(data(), global_topn())
      
      # 1) Pull in the raw table
      dt <- as.data.table(data())
      
      # 2) Figure out which categories are the top N
      N <- global_topn()  # this is just an integer
      sums <- dt[, .(total = sum(measurement_value)), by = category_var]
      top_cats <- sums[order(-total)][1:N, get(category_var)]
      
      # 3) Recode everything else as "Other"
      dt[, (category_var) := ifelse(
        get(category_var) %in% top_cats,
        get(category_var),
        "Other"
      )]
      
      # 4) Now aggregate by (possibly recoded) category and year
      df <- dt[, .(measurement_value = sum(measurement_value)),
               by = c(category_var, "year")]
      flog.info("Time series data after grouping: %s", head(df))
      df
    })
    
    
    output$plot_by_time_by_dim <- renderDygraph({
      df <- data_time_series()
      
      # 1) construire et filtrer votre palette
      full_pal <- getPalette(category_var)
      pres     <- unique(df[[category_var]])
      pal      <- full_pal[names(full_pal) %in% pres]
      # pas besoin de réordonner, on utilisera dySeries() avec les noms
      # 2) passer en wide + xts
      df_wide <- dcast(
        df,
        year ~ get(category_var),
        value.var = "measurement_value",
        fill = 0
      )
      ts_xts <- xts(
        df_wide[, -1, with = FALSE],
        order.by = as.Date(paste0(df_wide$year, "-01-01"))
      )
      
      # 3) construire le graphique en précisant chaque série
      g <- dygraph(ts_xts) %>% dyOptions(fillGraph = TRUE)
      for (serie in colnames(ts_xts)) {
        # pal[serie] existe toujours, y compris pour "Other"
        g <- g %>% dySeries(serie, color = pal[serie])
      }
      g %>% dyRangeSelector()
    })
    
    
  })
}


