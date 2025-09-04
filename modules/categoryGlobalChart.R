categoryGlobalChartUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Pie",        plotlyOutput(ns("pie_chart")) %>% withSpinner()),
    tabPanel("Stacked",    plotOutput(ns("stacked_bar_absolute")) %>% withSpinner()),
    tabPanel("Relative",   plotOutput(ns("stacked_bar_relative")) %>% withSpinner()),
    tabPanel("Treemap",    plotOutput(ns("treemap_chart")) %>% withSpinner()), 
    tabPanel("Cumulative %",
             fluidPage(
               fluidRow(
                 column(6, selectInput(ns("cumul_dim"), "Group by (optional)", choices = variable_to_display)),
                 column(6, numericInput(ns("top_n_cat2"), "Max categories", value = 10, min = 1))
               ),
               plotOutput(ns("stacked_bar_split")) %>% withSpinner()
             )
    )
  )
}


categoryGlobalChartServer <- function(id, category, reactive_data, sql_query = NULL, global_topn, variable_to_display) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pal <- getPalette(category)
    # pal <- pal[reactive_data$cat2]
    # pal <- pal[names(pal) %in% reactive_data[[category]]]
    colors <- unname(pal)
    
    getTopCategories <- reactive({
      df <- data_raw()
      N <- global_topn()
      df %>%
        group_by(cat = .data[[category]]) %>%
        summarise(total = sum(measurement_value), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = N) %>%
        pull(cat)
    })
    
    
    data_raw <- reactive({
      if (!is.null(sql_query)) {
        query <- paste0(sprintf(
          "SELECT %s, time_start, sum(measurement_value) AS measurement_value FROM(", category),
          sql_query(),
          sprintf(") AS foo GROUP BY time_start, %s ORDER BY %s", category, category)
        )
        df <- st_read(pool, query = query)
      } else {
        req(reactive_data())
        reactive_data()
      }
    })
    
    data_topN <- reactive({
      req(data_raw(), global_topn())
      df <- data_raw()
      N <- global_topn()
      
      top_cats <- df %>%
        group_by(cat = .data[[category]]) %>%
        summarise(total = sum(measurement_value), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = N) %>%
        pull(cat)
      
      df %>%
        mutate(cat2 = ifelse(.data[[category]] %in% top_cats, .data[[category]], "Other"))
    })
    
    # -- Pie Chart --
    output$pie_chart <- renderPlotly({
      df <- data_topN() %>%
        group_by(cat2) %>%
        summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        rename(!!category := cat2)
      
      plot_ly(
        df,
        labels = as.formula(paste0("~`", category, "`")),
        values = ~measurement_value,
        type   = 'pie',
        marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))
      ) %>%
        layout(
          title = sprintf('Distribution for %s', category),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    # -- Stacked bar chart (absolute) --
    output$stacked_bar_absolute <- renderPlot({
      df <- data_topN() %>%
        group_by(time_start, cat2) %>%
        summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      df <- df %>%
        mutate(time_month = format(as.Date(time_start), "%Y-%m")) %>%
        mutate(time_month = factor(time_month, levels = sort(unique(time_month))))
      
      ggplot(df, aes(x =time_month, y = measurement_value, fill = cat2))  +
        scale_fill_manual(values = pal) +
        geom_bar(stat = "identity") +
        labs(x = "Time", y = "Total", fill = category) +
        theme_minimal()
    })
    
    # -- Stacked bar chart (relative) --
    output$stacked_bar_relative <- renderPlot({
      df <- data_topN() %>%
        group_by(time_start, cat2) %>%
        summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        group_by(time_start) %>%
        mutate(perc = measurement_value / sum(measurement_value)) %>%
        ungroup()
      
      ggplot(df, aes(x = as.Date(time_start), y = perc, fill = cat2)) +
        scale_fill_manual(values = pal) +
        geom_bar(stat = "identity") +
        labs(x = "Time", y = "Proportion", fill = category) +
        theme_minimal()
    })
    
    # -- Treemap --
    output$treemap_chart <- renderPlot({
      df <- data_topN() %>%
        group_by(cat2) %>%
        summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        mutate(pct = measurement_value / sum(measurement_value) * 100)
      
      ggplot(df, aes(
        area = measurement_value,
        fill = cat2,
        label = paste0(cat2, " (", round(pct, 1), "%)")
      )) +
        treemapify::geom_treemap() +
        treemapify::geom_treemap_text(reflow = TRUE, place = "centre") +
        labs(title = sprintf("Treemap of %s", category)) +   # assure-toi que `category` est défini
        theme_minimal()
    })
    
    observe({
      dims <- setdiff(variable_to_display, category)  # on enlève la variable déjà utilisée
      updateSelectInput(session, "cumul_dim", choices = c("None" = "", dims))
    })
    
    data_cumulative_split <- reactive({
      req(data_raw(), input$cumul_dim, input$top_n_cat2)
      df0 <- data_raw()
      dim2 <- input$cumul_dim
      top_n2 <- input$top_n_cat2
      
      # 1) Top N catégories principales (déjà géré)
      top1 <- getTopCategories()
      
      # 2) Filtré + recodé pour cat2
      df <- df0 %>%
        mutate(
          cat2 = ifelse(.data[[category]] %in% top1, 
                        .data[[category]], 
                        "Other")
        )
      
      # 3) Calcul des top N modalités de dim2 sur l'ensemble
      top2 <- df %>%
        group_by(dim2_tmp = .data[[dim2]]) %>%
        summarise(total2 = sum(measurement_value), .groups="drop") %>%
        arrange(desc(total2)) %>%
        slice_head(n = top_n2) %>%
        pull(dim2_tmp)
      
      # 4) Recodage de dim2 et agrégation
      df %>%
        mutate(dim2_agg = ifelse(.data[[dim2]] %in% top2, .data[[dim2]], "Other")) %>%
        group_by(cat2, dim2_agg) %>%
        summarise(measurement_value = sum(measurement_value), .groups="drop")
    })
    
    
    output$stacked_bar_split <- renderPlot({
      df   <- data_cumulative_split()
      req(df)
      
      # Totaux par cat2 pour la ligne cumulative
      total_by_cat <- df %>%
        group_by(cat2) %>%
        summarise(total = sum(measurement_value), .groups="drop") %>%
        arrange(desc(total)) %>%
        mutate(cum_perc = cumsum(total) / sum(total))
      
      # Palette pour dim2_agg
      full_pal2 <- getPalette(category)
      pres2     <- unique(df$dim2_agg)
      pal2      <- full_pal2[names(full_pal2) %in% pres2]
      if ("Other" %in% pres2 && !"Other" %in% names(pal2)) {
        pal2 <- c(pal2, Other = "#CCCCCC")
      }
      
      # Facteur pour ordonner les barres
      total_by_cat$cat2 <- factor(total_by_cat$cat2, levels = total_by_cat$cat2)
      
      ggplot() +
        # 1) Bar stackée
        geom_bar(
          data = df,
          aes(
            x = factor(cat2, levels = levels(total_by_cat$cat2)),
            y = measurement_value,
            fill = dim2_agg
          ),
          stat = "identity"
        ) +
        # 2) Courbe cumulative (à l’échelle du total max)
        geom_line(
          data = total_by_cat,
          aes(
            x = cat2,
            y = cum_perc * max(total_by_cat$total),
            group = 1
          ),
          color = "red",
          size = 1
        ) +
        geom_point(
          data = total_by_cat,
          aes(
            x = cat2,
            y = cum_perc * max(total_by_cat$total)
          ),
          color = "red",
          size = 2
        ) +
        # 3) Étiquettes % cumulé
        geom_text(
          data = total_by_cat,
          aes(
            x = cat2,
            y = cum_perc * max(total_by_cat$total),
            label = paste0(round(cum_perc * 100), "%")
          ),
          vjust = -0.5,
          size = 3,
          color = "red"
        ) +
        scale_fill_manual(
          name = input$cumul_dim,
          values = pal2
        ) +
        labs(
          x = category,
          y = "Measurement",
          title = sprintf(
            "Top %s %s stacked by %s + cumulative",
            input$top_n_cat2,
            category,
            input$cumul_dim
          )
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
    
    
  })
}
