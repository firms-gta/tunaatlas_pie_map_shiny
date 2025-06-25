pieMapTimeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId  = ns("map_mode"),
      label    = "Map mode :",
      choices  = c(
        "No map"      = "none",
        "Static"    = "static",
        "Interactive" = "interactive"
      ),
      selected = "static",
      inline   = TRUE
    ),
    # <- on n'affiche ce bloc que si on est en mode 'interactive'
    conditionalPanel(
      condition = sprintf("input['%s'] == 'interactive'", ns("map_mode")),
      div(
        style = "height: 300px; position: relative;",
        leafletOutput(ns("pie_map")) %>% withSpinner(),
        actionButton(
          inputId = ns("submit_draw_pie_map"),
          label   = "Update WKT from drawing",
          class   = "btn-primary",
          style   = paste(
            "position: absolute;",
            "top: 100px;",
            "right: 20px;",
            "z-index: 400;",
            "font-size: 0.8em;",
            "padding: 5px 10px;"
          )
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'interactive'", ns("map_mode")),
      uiOutput(ns("map_ui"))
    )
  )
}



pieMapTimeSeriesServer <- function(id, category_var, data, centroid, submitTrigger, newwkttest, geom, global_topn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    zoom_level <- reactiveVal(1)
    target_var <- getTarget(category_var)
    
    observeEvent(global_topn(), {
      # Force a zoom refresh to trigger leaflet redraw
      flog.info("Triggering zoom reset due to topn change")
      zoom_level(zoom_level())  # force la reactive à être invalide et donc rezoomer pour afficher les minicharts
    })
    
    data_pie_map <- reactive({
      flog.info("Generating pie map data for category: %s", category_var)
      req(data(),global_topn())
      # observeEvent(input$n_vars, {
      #   global_topn(input$n_vars)
      # }, ignoreInit = TRUE)
      dt <- as.data.table(data())
      
      # 1) Sum by category & geoid
      dt <- dt[, .(measurement_value = sum(measurement_value)),
               by = c(category_var, "geographic_identifier")]
      
      # 2) Compute global totals per category and pick top N
      totals <- dt[, .(grand = sum(measurement_value)), by = category_var][
        order(-grand)
      ]
      
      N <- global_topn()
      
      topn <- head(totals[[category_var]], N)
      
      # 3) Recode everything else as "Other"
      dt[!(get(category_var) %in% topn), (category_var) := "Other"]
      
      # 4) Re‐aggregate now that we have “Other”
      dt <- dt[, .(measurement_value = round(sum(measurement_value))),
               by = c(category_var, "geographic_identifier")]
      
      # 5) Pivot to wide
      dt_wide <- dcast(
        dt,
        geographic_identifier ~ get(category_var),
        value.var = "measurement_value",
        fill = 0
      )
      
      # 6) Total column
      dt_wide[, total := rowSums(.SD),
              .SDcols = setdiff(names(dt_wide), "geographic_identifier")]
      
      # 7) Join geometry
      geom_df <- geom() %>% dplyr::select(-gridtype)
      st_as_sf(dplyr::left_join(dt_wide, geom_df, by = "geographic_identifier"))
    })
    
    la_palette <- reactive({
      la_palette
      pal <- getPalette(category_var)
      pal[names(pal) %in% colnames(data_pie_map())]
    })
    # pas beosin de la sortir du module, c'est très rapide environ 19ms 
    
    output$map_ui <- renderUI({
      req(input$map_mode)
      switch(
        input$map_mode,
        none = NULL,
        static = tmapOutput(ns("pie_map_plot"), height = "400px"),
        interactive = leafletOutput(ns("pie_map"), height = "400px")
      )
    })
    
    output$pie_map_plot <- renderPlot({
      req(input$map_mode == "static")
      df <- data_pie_map()
      
      # Calcul des centroides fiables pour données géographiques
      cent <- sf::st_point_on_surface(df)
      coords <- sf::st_coordinates(cent)
      
      # Préparation du data.frame pour scatterpie
      vals <- df %>%
        sf::st_drop_geometry() %>%
        dplyr::select_if(is.numeric)
      plot_df <- cbind(
        as.data.frame(coords),
        vals
      )
      la_pal <- la_palette()
      # Tracé de la carte et des camemberts
      ggplot2::ggplot() +
        # Fond de carte
        geom_sf(data = world, fill = NA, color = "grey70", size = 0.2) +
        # ggplot2::geom_sf(data = df, fill = NA, color = "grey50") +
        # Camemberts avec écart réduit
        scatterpie::geom_scatterpie(
          aes(
            x = X,
            y = Y,
            # on réduit la disparité : racine de la proportion plutôt que proportion brute
            r = sqrt(total / max(total)) *5
          ),
          data = plot_df,
          cols = setdiff(names(plot_df), c("X", "Y", "total")),
          alpha = 0.8
        ) +
        # même palette que pour la carte interactive
        ggplot2::scale_fill_manual(values = la_pal) +
        coord_sf() +
        ggplot2::theme_minimal() 
      
    })
    
    
    # Interactive view avec tmap sans tmap_mode dans le rendu
    output$pie_map <- renderLeaflet({
      flog.info("Rendering pie map")
      req(data_pie_map(), zoom_level(), centroid())
      df       <- data_pie_map()
      ctr      <- st_as_sf(centroid())
      la_pal   <- la_palette()
      
      # 1) Extraire les colonnes de données pour les mini‐charts
      chartdata_df <- 
        df %>% 
        st_drop_geometry() %>% 
        dplyr::select(-total) %>% 
        dplyr::select_if(is.numeric)
      chart_cols <- colnames(chartdata_df)
      
      # 2) Filtrer et réordonner la palette pour coller à ces colonnes
      pal <- la_pal[names(la_pal) %in% chart_cols]
      pal <- pal[chart_cols]    # 
      pal_vec <- unname(pal)    # on enlève les noms
      
      leaflet() %>% 
        addProviderTiles("Esri.NatGeoWorldMap", group = "background") %>%
        setView(lng = st_coordinates(ctr)[1,1],
                lat = st_coordinates(ctr)[1,2],
                zoom = zoom_level()) %>%
        onRender(sprintf(
          "function(el,x){var map=this;map.on('zoomend',function(){Shiny.setInputValue('%smap_zoom_level',map.getZoom());});Shiny.setInputValue('%smap_zoom_level',map.getZoom());}",
          session$ns(""), session$ns("")
        )) %>%
        clearBounds() %>%
        addDrawToolbar(targetGroup = "draw",
                       editOptions = editToolbarOptions(selectedPathOptions())) %>%
        addLayersControl(overlayGroups = c("draw"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMinicharts(
          lng          = st_coordinates(st_centroid(df, crs=4326))[, "X"],
          lat          = st_coordinates(st_centroid(df, crs=4326))[, "Y"],
          maxValues    = max(df$total),
          chartdata    = chartdata_df,
          type         = "pie",
          colorPalette = pal_vec,    
          width        = 8 + ((zoom_level()*20)*(df$total/max(df$total))),
          legend       = TRUE,
          legendPosition = "bottomright",
          layerId      = "minicharts"
        ) %>%
        addLayersControl(baseGroups = c("minicharts","grid"),
                         overlayGroups = c("background"))
    })
    
    
    observeEvent(input$map_zoom_level, {
      flog.info("Updating zoom level to: %s", input$map_zoom_level)
      df <- data_pie_map()
      la_palette <- la_palette()
      
      # Calculate new width based on zoom level
      new_width <- 8 + (input$map_zoom_level * 20) * (df$total / max(df$total))
      
      # Update leaflet map with new minicharts
      leafletProxy("pie_map", data = df) %>%
        clearGroup("minicharts") %>% 
        addMinicharts(
          lng = st_coordinates(st_centroid(df, crs = 4326))[, "X"],
          lat = st_coordinates(st_centroid(df, crs = 4326))[, "Y"],
          maxValues = max(df$total),
          chartdata = df %>%
            st_drop_geometry() %>%
            dplyr::select(-total) %>%
            dplyr::select_if(is.numeric),
          type = "pie",
          colorPalette = unname(la_palette),
          transitionTime = 50,
          width = new_width,  
          legend = TRUE, 
          legendPosition = "bottomright"
        )
    })
    
    observeEvent(input$submit_draw_pie_map, {
      flog.info("Submitting draw")
      
      if (!is.null(input$pie_map_draw_new_feature) && 
          !is.null(input$pie_map_draw_new_feature$geometry) && 
          length(input$pie_map_draw_new_feature$geometry$coordinates) > 0) {
        
        showModal(modalDialog(
          title = "Changing spatial coverage",
          "Attention, you are about to change the geographic coverage of the filter, it can take some time. Are you sure?",
          footer = tagList(
            modalButton("No"),
            actionButton(ns("yes_button_pie_map"), "Yes")  # Ensure ns is used
          ),
          easyClose = TRUE,
          id = ns("confirmation_modal")  
        ))
      } else {
        showModal(modalDialog(
          title = "Changing spatial coverage",
          "Please draw a square or polygon shape",
          footer = tagList(
            modalButton("Ok"),
          ),
          easyClose = TRUE,
          id = ns("drawashape")  
        ))
      }
    })
    
    observeEvent(input$yes_button_pie_map, {
      flog.info("Yes button clicked cahnign the wkt")
      req(input$pie_map_draw_new_feature$geometry)
      req(input$pie_map_draw_stop)
      geojson <- input$pie_map_draw_new_feature$geometry
      # Convert GeoJSON to sf object
      geojson_text <- toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
      sf_obj <- geojsonsf::geojson_sf(geojson_text)
      
      
      # Convert to WKT
      wkt_val <- st_as_text(sf_obj$geometry)
      flog.info("wkt")
      removeModal()
      flog.info("submittrigger")
      
      newwkttest(wkt_val)
      # global_topn(input$n_vars)
      # shinyjs::click("submit")
      
    })
    
    # return(list(newwkt = newwkt))
    
  })
}
