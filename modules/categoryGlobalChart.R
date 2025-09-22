categoryGlobalChartUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Cumulative %",
             fluidPage(
               fluidRow(
                 column(6, selectInput(ns("cumul_dim"), "Group by (optional)", choices = variable_to_display)),
                 column(6, numericInput(ns("top_n_cat2"), "Max categories", value = 10, min = 1))
               ),
               plotOutput(ns("stacked_bar_split")) %>% withSpinner()
             )
    ),
    tabPanel("Stacked",    plotOutput(ns("stacked_bar_absolute")) %>% withSpinner()),
    tabPanel("Relative",   plotOutput(ns("stacked_bar_relative")) %>% withSpinner()),
    tabPanel("Pie",        plotlyOutput(ns("pie_chart")) %>% withSpinner()),
    tabPanel("Treemap",    plotOutput(ns("treemap_chart")) %>% withSpinner())
  )
}


categoryGlobalChartServer <- function(id, category, reactive_data, sql_query = NULL, global_topn, variable_to_display) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pal <- getPalette(category)
    
    # last_good_topn <- shiny::reactiveVal(10)
    safe_topn <- global_topn
    # safe_topn <- shiny::reactive({
    #   val <- tryCatch({
    #     if (shiny::is.reactive(global_topn)) global_topn() else global_topn
    #   }, error = function(e) NA_real_)   # shiny.silent.error => NA
    #   
    #   # validation : numeric, fini, >=1
    #   if (is.null(val) || length(val) == 0 || !is.numeric(val) || !is.finite(val) || val < 1) {
    #     flog.warn("[%s] global_topn invalid -> fallback last_good_topn=%s",
    #               session$ns("dbg"), last_good_topn())
    #     return(last_good_topn())
    #   } else {
    #     last_good_topn(val)
    #     return(val)
    #   }
    # })
    
    
    # pal <- pal[reactive_data$cat2]
    # pal <- pal[names(pal) %in% reactive_data[[category]]]
    colors <- unname(pal)
    
    getTopCategories <- reactive({
      df <- data_raw()
      N <- safe_topn()
      df %>%
        dplyr::group_by(cat = .data[[category]]) %>%
        dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::slice_head(n = N) %>%
        dplyr::pull(cat)
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
      req(data_raw())
      df <- data_raw()
      N <- safe_topn()
      
      top_cats <- df %>%
        dplyr::group_by(cat = .data[[category]]) %>%
        dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
        dplyr::arrange(desc(total)) %>%
        dplyr::slice_head(n = N) %>%
        dplyr::pull(cat)
      
      df %>%
        dplyr::mutate(cat2 = ifelse(.data[[category]] %in% top_cats, .data[[category]], "Other"))
    })
    
    # -- Pie Chart --
    output$pie_chart <- renderPlotly({
      df <- data_topN() %>%
        dplyr::group_by(cat2) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        dplyr::rename(!!category := cat2)
      
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
        dplyr::group_by(time_start, cat2) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      df <- df %>%
        dplyr::mutate( time_start = as.Date(substr(as.character(time_start), 1, 10))) %>% 
        dplyr::mutate(time_month = format(as.Date(time_start), "%Y-%m")) %>%
        dplyr::mutate(time_month = factor(time_month, levels = sort(unique(time_month))))
      
      ggplot(df, aes(x =time_month, y = measurement_value, fill = cat2))  +
        scale_fill_manual(values = pal) +
        geom_bar(stat = "identity") +
        labs(x = "Time", y = "Total", fill = category) +
        theme_minimal()
    })
    
    # -- Stacked bar chart (relative) --
    output$stacked_bar_relative <- renderPlot({
      df <- data_topN() %>%
        dplyr::group_by(time_start, cat2) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        dplyr::group_by(time_start) %>%
        dplyr::mutate(perc = measurement_value / sum(measurement_value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          time_start = as.Date(substr(as.character(time_start), 1, 10))
        )
      
      ggplot(df, aes(x = as.Date(time_start), y = perc, fill = cat2)) +
        scale_fill_manual(values = pal) +
        geom_bar(stat = "identity") +
        labs(x = "Time", y = "Proportion", fill = category) +
        theme_minimal()
    })
    
    # -- Treemap --
    output$treemap_chart <- renderPlot({
      df <- data_topN() %>%
        dplyr::group_by(cat2) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
        dplyr::mutate(pct = measurement_value / sum(measurement_value) * 100)
      
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
        dplyr::group_by(dim2_tmp = .data[[dim2]]) %>%
        dplyr::summarise(total2 = sum(measurement_value), .groups="drop") %>%
        dplyr::arrange(desc(total2)) %>%
        dplyr::slice_head(n = top_n2) %>%
        dplyr::pull(dim2_tmp)
      
      # 4) Recodage de dim2 et agrégation
      df %>%
        dplyr::mutate(dim2_agg = ifelse(.data[[dim2]] %in% top2, .data[[dim2]], "Other")) %>%
        dplyr::group_by(cat2, dim2_agg) %>%
        dplyr::summarise(measurement_value = sum(measurement_value), .groups="drop")
    })
    
    
    output$stacked_bar_split <- renderPlot({
      df <- data_cumulative_split()
      req(df)
      
      # Totaux par catégorie (axe x) + cumul
      totals <- df %>%
        dplyr::group_by(cat2) %>%
        dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total)) %>%
        dplyr::mutate(cum_perc = cumsum(total) / sum(total))
      
      # Niveaux d'ordre communs pour toutes les couches
      x_levels <- totals$cat2
      df <- df %>% dplyr::mutate(x = factor(cat2, levels = x_levels))
      totals <- totals %>%
        dplyr::mutate(x = factor(cat2, levels = x_levels),
                      cum_y = cum_perc * max(total))
      
      pres2 <- df %>% dplyr::distinct(dim2_agg) %>% dplyr::pull(dim2_agg) %>% as.character()
      df <- df %>% dplyr::mutate(dim2_agg = factor(dim2_agg, levels = pres2))
      
      full_pal2 <- getPalette(input$cumul_dim)             
      pal2 <- full_pal2[pres2]                              
      if (any(is.na(pal2))) {                               # repli si palettes manquantes
        pal2 <- setNames(scales::hue_pal()(length(pres2)), pres2)
      }
      if ("Other" %in% pres2) pal2["Other"] <- "#CCCCCC"    # couleur dédiée à "Other"
      
      ggplot() +
        geom_col(data = df, aes(x = x, y = measurement_value, fill = dim2_agg)) +
        geom_line(data = totals, aes(x = x, y = cum_y, group = 1), color = "red", linewidth = 1) +
        geom_point(data = totals, aes(x = x, y = cum_y), color = "red", size = 2) +
        geom_text(data = totals,
                  aes(x = x, y = cum_y, label = paste0(round(cum_perc*100), "%")),
                  vjust = -0.5, size = 3, color = "red") +
        scale_fill_manual(name = input$cumul_dim, values = pal2, breaks = pres2, drop = FALSE) +
        labs(
          x = category, y = "Measurement",
          title = sprintf("Top %s %s stacked by %s + cumulative", input$top_n_cat2, category, input$cumul_dim)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    
    
    
  })
}
