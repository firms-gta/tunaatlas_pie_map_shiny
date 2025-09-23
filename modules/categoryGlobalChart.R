categoryGlobalChartUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Cumulative %",
             fluidPage(
               fluidRow(
                 column(6, selectInput(ns("cumul_dim"), "Group by (optional)", choices = NULL)),  # <- NULL
                 column(6, numericInput(ns("top_n_cat2"), "Max categories (for the groupping variable)", value = 10, min = 1))
               ),
               plotOutput(ns("stacked_bar_split")) %>% withSpinner()
             )
    ),
    tabPanel("Stacked",   plotOutput(ns("stacked_bar_absolute")) %>% withSpinner()),
    tabPanel("Relative",  plotOutput(ns("stacked_bar_relative")) %>% withSpinner()),
    tabPanel("Pie",       plotlyOutput(ns("pie_chart")) %>% withSpinner()),
    tabPanel("Treemap",   plotOutput(ns("treemap_chart")) %>% withSpinner())
  )
}

categoryGlobalChartServer <- function(id, category, reactive_data, sql_query = NULL, global_topn, variable_to_display) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    value_of <- function(x) if (is.function(x)) x() else x
    
    # --- données brutes
    data_raw <- reactive({
      if (!is.null(sql_query)) {
        query <- paste0(sprintf(
          "SELECT %s, time_start, sum(measurement_value) AS measurement_value FROM(", value_of(category)),
          sql_query(),
          sprintf(") AS foo GROUP BY time_start, %s ORDER BY %s", value_of(category), value_of(category))
        )
        df <- st_read(pool, query = query)
      } else {
        req(reactive_data())
        reactive_data()
      }
    })
  
  # --- top N catégories (pour recoder 'Other')
  getTopCategories <- reactive({
    df <- req(data_raw())
    N  <- req(if (is.function(global_topn)) global_topn() else global_topn)
    cat_col <- value_of(category)
    df %>%
      dplyr::group_by(cat = .data[[cat_col]]) %>%
      dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::slice_head(n = N) %>%
      dplyr::pull(cat)
  })
  
  # --- data avec cat2 (topN + 'Other')
  data_topN <- reactive({
    df <- req(data_raw())
    N  <- req(if (is.function(global_topn)) global_topn() else global_topn)
    cat_col <- value_of(category)
    
    top_cats <- df %>%
      dplyr::group_by(cat = .data[[cat_col]]) %>%
      dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::slice_head(n = N) %>%
      dplyr::pull(cat)
    
    df %>%
      dplyr::mutate(cat2 = ifelse(.data[[cat_col]] %in% top_cats, .data[[cat_col]], "Other"))
  })
  
  # --- PALETTE UNIFIÉE pour les catégories (cat2)
  pal_cat <- reactive({
    base <- getPalette(value_of(category))    # toutes les couleurs possibles pour cette dimension
    # catégories réellement présentes (dans cat2, donc topN + 'Other')
    present <- req(data_topN()) %>% dplyr::distinct(cat2) %>% dplyr::pull(cat2) %>% as.character()
    pal <- base[present]
    # si 'Other' n’est pas dans base -> gris
    if ("Other" %in% present && (is.na(pal["Other"]) || is.null(pal["Other"]))) pal["Other"] <- "#CCCCCC"
    unname(pal)[match(present, names(pal))] -> colors  # ordre stable == 'present'
    stats::setNames(colors, present)
  })
  
  # --- garder "None" persistant + options dynamiques
  observe({
    dims_all <- if (is.function(variable_to_display)) variable_to_display() else variable_to_display
    dims <- setdiff(dims_all, value_of(category))
    current <- isolate(input$cumul_dim)
    choices <- c("None" = "", dims)
    updateSelectInput(session, "cumul_dim", choices = choices, selected = if (!is.null(current) && current %in% c("", dims)) current else "")
  })
  
  # ---- PIE
  output$pie_chart <- renderPlotly({
    cat_col <- value_of(category)
    pal <- pal_cat()
    colors <- unname(pal)
    
    df <- data_topN() %>%
      dplyr::group_by(cat2) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
      dplyr::rename(!!cat_col := cat2)
    
    # réaligner les couleurs à l’ordre des labels
    lbls <- df[[cat_col]]
    colmap <- pal[names(pal) %in% lbls]; colmap <- colmap[lbls]
    
    plot_ly(
      df,
      labels = as.formula(paste0("~`", cat_col, "`")),
      values = ~measurement_value,
      type   = "pie",
      marker = list(colors = unname(colmap), line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        title = sprintf("Distribution for %s", cat_col),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  # ---- STACKED ABSOLUTE
  output$stacked_bar_absolute <- renderPlot({
    pal <- pal_cat()
    df <- data_topN() %>%
      dplyr::group_by(time_start, cat2) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
      dplyr::mutate(
        time_start = as.Date(substr(as.character(time_start), 1, 10)),
        time_month = factor(format(time_start, "%Y-%m"),
                            levels = sort(unique(format(time_start, "%Y-%m"))))
      )
    
    # ~12 ticks max sur l’axe 
    all_x <- levels(df$time_month)
    step  <- max(1L, floor(length(all_x) / 12L))
    x_breaks <- all_x[seq(1, length(all_x), by = step)]
    
    ggplot2::ggplot(df, ggplot2::aes(x = time_month, y = measurement_value, fill = cat2)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = pal, na.translate = FALSE) +
      ggplot2::scale_x_discrete(breaks = x_breaks, guide = ggplot2::guide_axis(angle = 45)) +
      ggplot2::labs(x = "Time", y = "Total", fill = value_of(category)) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(hjust = 1),
        panel.grid.minor = ggplot2::element_blank()
      )
  })
  
  # ---- STACKED RELATIVE
  output$stacked_bar_relative <- renderPlot({
    pal <- pal_cat()
    df <- data_topN() %>%
      dplyr::group_by(time_start, cat2) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
      dplyr::group_by(time_start) %>%
      dplyr::mutate(perc = measurement_value / sum(measurement_value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_start = as.Date(substr(as.character(time_start), 1, 10)))
    
    ggplot2::ggplot(df, ggplot2::aes(x = time_start, y = perc, fill = cat2)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = pal, na.translate = FALSE) +
      ggplot2::labs(x = "Time", y = "Proportion", fill = value_of(category)) +
      ggplot2::theme_minimal()
  })
  
  # ---- TREEMAP (même palette catégories)
  output$treemap_chart <- renderPlot({
    pal <- pal_cat()
    df <- data_topN() %>%
      dplyr::group_by(cat2) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
      dplyr::mutate(pct = measurement_value / sum(measurement_value) * 100)
    
    pres <- df$cat2
    pal2 <- pal[names(pal) %in% pres]; pal2 <- pal2[pres]
    
    ggplot2::ggplot(df, ggplot2::aes(
      area = measurement_value, fill = cat2,
      label = paste0(cat2, " (", round(pct, 1), "%)")
    )) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(reflow = TRUE, place = "centre") +
      ggplot2::scale_fill_manual(values = pal2, na.translate = FALSE) +
      ggplot2::labs(title = sprintf("Treemap of %s", value_of(category))) +
      ggplot2::theme_minimal()
  })
  
  # ---- Données pour "Cumulative %"
  data_cumulative_split <- reactive({
    df0   <- req(data_raw())
    dim2  <- input$cumul_dim %||% ""
    top_n2 <- input$top_n_cat2 %||% 10L
    cat_col <- value_of(category)
    
    top1 <- getTopCategories()
    
    df <- df0 %>%
      dplyr::mutate(cat2 = ifelse(.data[[cat_col]] %in% top1, .data[[cat_col]], "Other"))
    
    if (identical(dim2, "") || !dim2 %in% names(df)) {
      # (cas None) -> on renvoie juste les totaux par cat2
      return(
        df %>% dplyr::group_by(cat2) %>%
          dplyr::summarise(measurement_value = sum(measurement_value), .groups="drop") %>%
          dplyr::mutate(dim2_agg = "(all)")
      )
    }
    
    top2 <- df %>%
      dplyr::group_by(dim2_tmp = .data[[dim2]]) %>%
      dplyr::summarise(total2 = sum(measurement_value), .groups="drop") %>%
      dplyr::arrange(desc(total2)) %>%
      dplyr::slice_head(n = top_n2) %>%
      dplyr::pull(dim2_tmp)
    
    df %>%
      dplyr::mutate(dim2_agg = ifelse(.data[[dim2]] %in% top2, .data[[dim2]], "Other")) %>%
      dplyr::group_by(cat2, dim2_agg) %>%
      dplyr::summarise(measurement_value = sum(measurement_value), .groups="drop")
  })
  
  # ---- "Cumulative %" plot
  output$stacked_bar_split <- renderPlot({
    df <- req(data_cumulative_split())
    pal_cat2 <- pal_cat()
    
    # ordre des catégories (x)
    totals <- df %>%
      dplyr::group_by(cat2) %>%
      dplyr::summarise(total = sum(measurement_value), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::mutate(cum_perc = cumsum(total) / sum(total))
    
    x_levels <- totals$cat2
    totals <- totals %>%
      dplyr::mutate(x = factor(cat2, levels = x_levels),
                    cum_y = cum_perc * max(total))
    
    # --- cas None (pas de group by) -> une couleur par catégorie (palette principale)
    if (!nzchar(input$cumul_dim %||% "")) {
      df_single <- df %>% dplyr::mutate(x = factor(cat2, levels = x_levels))
      pal_x <- pal_cat2[names(pal_cat2) %in% x_levels]; pal_x <- pal_x[x_levels]
      
      return(
        ggplot2::ggplot() +
          ggplot2::geom_col(data = df_single, ggplot2::aes(x = x, y = measurement_value, fill = cat2)) +
          ggplot2::geom_line(data = totals, ggplot2::aes(x = x, y = cum_y, group = 1), color = "red", linewidth = 1) +
          ggplot2::geom_point(data = totals, ggplot2::aes(x = x, y = cum_y), color = "red", size = 2) +
          ggplot2::geom_text(
            data = totals,
            ggplot2::aes(x = x, y = cum_y, label = paste0(round(cum_perc*100), "%")),
            vjust = -0.5, size = 3, color = "red"
          ) +
          ggplot2::scale_fill_manual(name = "(all)", values = pal_x, breaks = x_levels, drop = FALSE) +
          ggplot2::labs(x = value_of(category), y = "Measurement",
                        title = sprintf("Top %s %s + cumulative", input$top_n_cat2 %||% 10L, value_of(category))) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .05))) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      )
    }
    
    # --- cas avec group by (dim2_agg)
    df <- df %>% dplyr::mutate(x = factor(cat2, levels = x_levels))
    pres2 <- df %>% dplyr::distinct(dim2_agg) %>% dplyr::pull(dim2_agg) %>% as.character()
    
    full_pal2 <- getPalette(input$cumul_dim)
    pal2 <- full_pal2[pres2]
    if (any(is.na(pal2)) || !length(pal2)) pal2 <- stats::setNames(scales::hue_pal()(length(pres2)), pres2)
    if ("Other" %in% pres2) pal2["Other"] <- "#CCCCCC"
    
    ggplot2::ggplot() +
      ggplot2::geom_col(data = df, ggplot2::aes(x = x, y = measurement_value, fill = dim2_agg)) +
      ggplot2::geom_line(data = totals, ggplot2::aes(x = x, y = cum_y, group = 1), color = "red", linewidth = 1) +
      ggplot2::geom_point(data = totals, ggplot2::aes(x = x, y = cum_y), color = "red", size = 2) +
      ggplot2::geom_text(
        data = totals,
        ggplot2::aes(x = x, y = cum_y, label = paste0(round(cum_perc*100), "%")),
        vjust = -0.5, size = 3, color = "red"
      ) +
      ggplot2::scale_fill_manual(name = input$cumul_dim %||% "(all)", values = pal2, breaks = pres2, drop = FALSE) +
      ggplot2::labs(
        x = value_of(category), y = "Measurement",
        title = sprintf("Top %s %s stacked by %s + cumulative",
                        input$top_n_cat2 %||% 10L, value_of(category), input$cumul_dim)
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .05))) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  })
}

