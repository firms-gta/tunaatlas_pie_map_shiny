pieMapTimeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("map_mode"),
      label   = "Map mode:",
      choices = c("Static (one map per gridtype)" = "static",
                  "Interactive (slower for global data)" = "interactive"),
      inline  = TRUE
    ),
    
    # Static mode: show both maps
    conditionalPanel(
      condition = sprintf("input['%s'] == 'static'", ns("map_mode")),
      fluidRow(
        column(6, h4("5deg grid"), uiOutput(ns("map_ui_5deg"))),
        column(6, h4("1deg grid"), uiOutput(ns("map_ui_1deg")))
      )
    ),
    
    # Interactive mode: show one combined map
    conditionalPanel(
      condition = sprintf("input['%s'] == 'interactive'", ns("map_mode")),
      fluidRow(
        column(12, h4("Interactive map (can be slow if multiples gridtypes)"),
               actionButton(ns("submit_draw_pie_map"), "Update wkt from drawing",
                            class = "btn-primary",
                            style = "position: absolute; top: 100px; right: 20px; z-index: 400; font-size: 0.8em; padding: 5px 10px;"),
               uiOutput(ns("map_ui_combined"))
        )
      )
    )
  )
}


## Server: split data by first digit of geographic_identifier
pieMapTimeSeriesServer <- function(id, category_var, data, data_witout_geom_, submitTrigger,
                                   newwkttest, geom, global_topn, map_mode_val, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    value_of <- function(x) if (is.function(x)) x() else x   # <- helper
    
    zoom_level <- reactiveVal(1)
    
    target_var <- reactive({
      getTarget(value_of(category_var))
    })
    require(promises); require(future)
    
    observe({
      updateRadioButtons(
        session, "map_mode",
        selected = map_mode_val()
      )
    })
    
    observeEvent(input$map_mode, {
      map_mode_val(input$map_mode)
    })
    observeEvent(submitTrigger(), {
      req(enabled())
      # Force a zoom refresh to trigger leaflet redraw
      flog.info("Triggering zoom reset due to topn change")
      zoom_level(zoom_level()+1)  # force la reactive à être invalide et donc rezoomer pour afficher les minicharts
    })
    
    
    observeEvent(global_topn(), {
      req(enabled())
      # Force a zoom refresh to trigger leaflet redraw
      flog.info("Triggering zoom reset due to topn change")
      zoom_level(zoom_level()+1)  # force la reactive à être invalide et donc rezoomer pour afficher les minicharts
    })
    
    # keep existing reactive and palette
    data_pie_map <- reactive({
      req(enabled(), data_witout_geom_(), global_topn(), data())
      flog.info("Generating pie map data for category: %s", value_of(category_var))
      dt <- data.table::as.data.table(data_witout_geom_())
      
      cat_col <- value_of(category_var)     # "species", "gear_type", etc.
      
      # 1) Somme par catégorie & geoid
      dt <- dt[, .(measurement_value = sum(measurement_value)),
               by = c(cat_col, "geographic_identifier")]
      
      # 2) Top N global
      totals <- dt[, .(grand = sum(measurement_value)), by = cat_col][order(-grand)]
      N <- global_topn()
      topn <- head(totals[[cat_col]], N)
      
      # 3) Recode "Other"
      dt[!(get(cat_col) %in% topn), (cat_col) := "Other"]
      
      # 4) Regrouper
      dt <- dt[, .(measurement_value = round(sum(measurement_value))),
               by = c(cat_col, "geographic_identifier")]
      
      # 5) Wide
      fml <- stats::as.formula(paste("geographic_identifier ~", cat_col))
      dt_wide <- data.table::dcast(dt, fml, value.var = "measurement_value", fill = 0)
      
      # 6) Total
      data_cols <- setdiff(names(dt_wide), "geographic_identifier")
      dt_wide[, total := rowSums(.SD), .SDcols = data_cols]
      
      # 7) Join centroids (X,Y) -> retourne bien quelque chose !
      geom_df <- dt_wide %>%
        dplyr::left_join(centroids, by = c("geographic_identifier"))
      
      geom_df   # <- IMPORTANT: retourner l’objet
    })

    la_palette <- reactive({
      req(enabled())
      pal <- getPalette(value_of(category_var))     # prend une string
      cols <- colnames(data_pie_map())
      pal[names(pal) %in% cols]
    })
    
    data_pie_map_5deg <- reactive({
      req(enabled())
      df <- data_pie_map()
      df <- df[substr(df$geographic_identifier, 1, 1) == "6", ]
      df$Y <- as.numeric(df$Y)
      df
    })
    
    data_pie_map_1deg <- reactive({
      req(enabled())
      df <- data_pie_map()
      df <- df[substr(df$geographic_identifier, 1, 1) == "5", ]
      df$Y <- as.numeric(df$Y)
      df
    })
    
    
    ### UI renderers for each map
    output$map_ui_5deg <- renderUI({
      req(enabled())
      req(input$map_mode)
      switch(input$map_mode,
             static      = withSpinner(tmap::tmapOutput(ns("pie_map_plot_5deg"), height = "400px")),
             interactive = withSpinner(leafletOutput(ns("pie_map_5deg"), height = "400px"))
      )
    })
    
    output$map_ui_1deg <- renderUI({
      req(enabled())
      req(input$map_mode)
      switch(input$map_mode,
             static      = withSpinner(tmap::tmapOutput(ns("pie_map_plot_1deg"), height = "400px")),
             interactive = withSpinner(leafletOutput(ns("pie_map_1deg"), height = "400px"))
      )
    })
    
    ### Static plots
    render_pie_plot <- function(df, pal) {
      req(enabled())
      if (!exists("world", inherits = TRUE)) {
        flog.info("Reloading world")
        world <<- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
      }
      plot_df <- df %>% dplyr::select(-geographic_identifier) %>%
        dplyr::mutate(r = scales::rescale(total, to = c(1,5), from = range(total, na.rm=TRUE)))
      pie_cols <- setdiff(names(plot_df), c("X","Y","total","r"))
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = world, fill = "antiquewhite", color = "grey70", linewidth = 0.2) +
        scatterpie::geom_scatterpie(ggplot2::aes(x = X, y = Y, r = r), data = plot_df,
                                    cols = pie_cols, colour = NA, alpha = 0.8) +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::coord_sf() + ggplot2::theme_minimal()
    }
    
    output$pie_map_plot_5deg <- renderPlot({
      req(enabled(), input$map_mode == "static")
      render_pie_plot(data_pie_map_5deg(), la_palette())
    })
    
    output$pie_map_plot_1deg <- renderPlot({
      req(enabled(), input$map_mode == "static")
      render_pie_plot(data_pie_map_1deg(), la_palette())
    })
    
    
    data_pie_map_combined <- reactive({
      req(enabled())
      req(input$map_mode == "interactive")
      rbind(
        data_pie_map_1deg(),
        data_pie_map_5deg()
      )
    })
    
    output$pie_map_combined <- renderLeaflet({
      req(enabled())
      req(input$map_mode == "interactive")
      flog.info("Rendering pie map")
      req(data_pie_map(), zoom_level())
      future({
        df       <- data_pie_map()
        # ctr      <- st_as_sf(centroid())
        la_pal   <- la_palette()
        
        # 1) Extraire les colonnes de données pour les mini‐charts
        chartdata_df <- 
          df %>% 
          st_drop_geometry() %>% 
          dplyr::select(-c(total, X, Y)) %>% 
          dplyr::select_if(is.numeric)
        chart_cols <- colnames(chartdata_df)
        
        # 2) Filtrer et réordonner la palette pour coller à ces colonnes
        pal <- la_pal[names(la_pal) %in% chart_cols]
        pal <- pal[chart_cols]    # 
        pal_vec <- unname(pal)    # on enlève les noms
        
        
        # geom_df <- geom() %>% dplyr::select(-gridtype)
        # df <- st_as_sf(dplyr::left_join(df %>% dplyr::select(-c(X,Y)), geom_df, by = "geographic_identifier"))
        # lngs     <- df$X
        # lats     <- df$Y
        # center_lon <- mean(lngs, na.rm = TRUE)
        # center_lat <- mean(lats, na.rm = TRUE)
        leaflet() %>% 
          addProviderTiles("Esri.NatGeoWorldMap", group = "background") %>%
          # setView(
          #   lng = center_lon,
          #   lat = center_lat,
          #   zoom = zoom_level()) %>%
          onRender(sprintf(
            "function(el,x){var map=this;map.on('zoomend',function(){Shiny.setInputValue('%smap_zoom_level',map.getZoom());});Shiny.setInputValue('%smap_zoom_level',map.getZoom());}",
            session$ns(""), session$ns("")
          )) %>%
          # clearBounds() %>%
          # addDrawToolbar(targetGroup = "draw",
          #                editOptions = editToolbarOptions(selectedPathOptions())) %>%
          # addLayersControl(overlayGroups = c("draw"),
          #                  options = layersControlOptions(collapsed = FALSE)) %>%
          # addMinicharts(
          #   lng          = st_coordinates(st_centroid(df, crs=4326))[, "X"],
          #   lat          = st_coordinates(st_centroid(df, crs=4326))[, "Y"],
          #   maxValues    = max(df$total),
          #   chartdata    = chartdata_df,
          #   type         = "pie",
          #   colorPalette = pal_vec,    
          #   width        = 8 + ((zoom_level()*20)*(df$total/max(df$total))),
          #   legend       = TRUE,
          #   legendPosition = "bottomright",
          #   layerId      = "minicharts"
          # ) %>%
          addLayersControl(baseGroups = c("minicharts","grid"),
                           overlayGroups = c("background"))}) %...>% identity()
    })
    
    
    observeEvent(input$map_zoom_level, {
      req(enabled())
      flog.info("Updating zoom level to: %s", input$map_zoom_level)
      df       <- data_pie_map()
      # ctr      <- st_as_sf(centroid())
      la_pal   <- la_palette()
      
      # 1) Extraire les colonnes de données pour les mini‐charts
      chartdata_df <- 
        df %>% 
        st_drop_geometry() %>% 
        dplyr::select(-c(total, X, Y)) %>% 
        dplyr::select_if(is.numeric)
      chart_cols <- colnames(chartdata_df)
      
      # 2) Filtrer et réordonner la palette pour coller à ces colonnes
      pal <- la_pal[names(la_pal) %in% chart_cols]
      pal <- pal[chart_cols]    # 
      pal_vec <- unname(pal)    # on enlève les noms
      geom_df <- geom() %>% dplyr::select(-gridtype)
      df <- st_as_sf(dplyr::left_join(df %>% dplyr::select(-c(X,Y)), geom_df, by = "geographic_identifier"))
      
      new_width <- 8 + (input$map_zoom_level * 20) * (df$total / max(df$total))
      
      leafletProxy("pie_map_combined", data = df) %>%  # <-- ici
        clearGroup("minicharts")%>%
        # clearBounds() %>%
        addDrawToolbar(targetGroup = "draw",
                       editOptions = editToolbarOptions(selectedPathOptions())) %>%
        addLayersControl(overlayGroups = c("draw"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        addMinicharts(
          lng = st_coordinates(st_centroid(df, crs = 4326))[, "X"],
          lat = st_coordinates(st_centroid(df, crs = 4326))[, "Y"],
          maxValues = max(df$total),
          chartdata = chartdata_df,
          type = "pie",
          colorPalette = pal_vec,
          transitionTime = 50,
          width = new_width,  
          legend = TRUE, 
          legendPosition = "bottomright"
        )
    })
    
    
    
    output$map_ui_combined <- renderUI({
      req(enabled())
      req(input$map_mode == "interactive")
      withSpinner(leafletOutput(ns("pie_map_combined"), height = "400px"))
    })
    
    
    observeEvent(input$submit_draw_pie_map, {
      req(enabled())
      flog.info("Submitting draw")
      
      feature <- input$pie_map_combined_draw_new_feature
      if (!is.null(feature$geometry) && length(feature$geometry$coordinates) > 0) {
        showModal(modalDialog(
          title = "Changing spatial coverage",
          "Attention, this will affect all maps. Continue?",
          footer = tagList(
            modalButton("No"),
            actionButton(ns("yes_button_pie_map"), "Yes")
          ),
          easyClose = TRUE,
          id = ns("confirmation_modal")
        ))
      } else {
        showModal(modalDialog(
          title = "No shape",
          "Please draw a shape on the map",
          footer = tagList(modalButton("OK")),
          easyClose = TRUE
        ))
      }
    })
    
    observeEvent(input$yes_button_pie_map, {
      req(enabled())
      req(input$pie_map_combined_draw_new_feature$geometry)
      geojson <- input$pie_map_combined_draw_new_feature$geometry
      geojson_text <- toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
      sf_obj <- geojsonsf::geojson_sf(geojson_text)
      wkt_val <- st_as_text(sf_obj$geometry)
      
      removeModal()
      flog.info("WKT applied to both maps")
      
      newwkttest(wkt_val)
    })
    
    
  })
}
