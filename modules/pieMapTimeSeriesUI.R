pieMapTimeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "height: 400px;",
      leafletOutput(ns("pie_map"))%>% withSpinner(),
      actionButton(ns("submit_draw_pie_map"), "Update wkt from drawing",
                   class = "btn-primary",
                   style = "position: absolute; top: 100px; right: 20px; z-index: 400; font-size: 0.8em; padding: 5px 10px;")
    ),
  )
}


pieMapTimeSeriesServer <- function(id, category_var, data, centroid, submitTrigger, newwkttest, geom) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    zoom_level <- reactiveVal(1)
    target_var <- getTarget(category_var)
    
    data_pie_map <- reactive({
      flog.info("Generating pie map data for category: %s", category_var)
      req(data())
      # Convert to data.table
      # dt <- as.data.table(data()[, setdiff(names(data()), c("geom_wkt", "geom"))])
      dt <- as.data.table(data())
      
      dt <- dt[, .(measurement_value = sum(measurement_value)), by = c(category_var, "geographic_identifier")]
      # Spread the data to wide format
      dt_wide <- dcast(dt, geographic_identifier ~ get(category_var), value.var = "measurement_value", fill = 0)
      target_var <- intersect(target_var, colnames(dt_wide))
      # Add total column by summing across the target variables
      dt_wide[, total := rowSums(.SD), .SDcols = target_var]
      
      # Join geometry back using the identifier column
      geometry_data <- geom() %>% dplyr::select(-gridtype)
      # geometry_data <- unique(data()[, .(geographic_identifier, geom_wkt)])
      dt_wide <- as.data.frame(dt_wide)
      dt_wide <- st_as_sf(dt_wide %>% dplyr::left_join(geometry_data))
      # dt_wide <- merge(dt_wide, geometry_data, by = "geographic_identifier", all.x = TRUE)
      
      flog.info("Pie map data created")
      dt_wide
    })
    
    
    la_palette <- reactive({
      flog.info("Generating palette for category: %s", category_var)
      la_palette <- getPalette(category_var)
      la_palette <- la_palette[names(la_palette) %in% colnames(data_pie_map())]
      flog.info("Palette: %s", la_palette)
      la_palette
    })
    # pas beosin de la sortir du module, c'est très rapide environ 19ms 
    
    
    
    output$pie_map <- renderLeaflet({
      flog.info("Rendering pie map")
      req(data_pie_map(), zoom_level())  
      req(centroid())
      df <- data_pie_map()
      centroid <- st_as_sf(centroid())
      lat_centroid <- st_coordinates(centroid)[2]
      lon_centroid <- st_coordinates(centroid)[1]
      la_palette <- la_palette()
      leaflet() %>% 
        addProviderTiles("Esri.NatGeoWorldMap", group = "background") %>%
        setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom_level()) %>%
        onRender(
          sprintf(
            "function(el, x) {
      var map = this;
      map.on('zoomend', function() {
        console.log('Zoom level:', map.getZoom());
        Shiny.setInputValue('%smap_zoom_level', map.getZoom(), {priority: 'event'});
      });
      
      Shiny.setInputValue('%smap_zoom_level', map.getZoom(), {priority: 'event'});
    }", session$ns(""), session$ns("")
          )
        ) %>% 
        clearBounds() %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        ) %>%
        addLayersControl(
          overlayGroups = c("draw"),
          options = layersControlOptions(collapsed = FALSE)
        )  %>% 
        addMinicharts(lng = st_coordinates(st_centroid(df, crs = 4326))[, "X"],
                      lat = st_coordinates(st_centroid(df, crs = 4326))[, "Y"],
                      maxValues = max(df$total),
                      chartdata = df %>%
                        st_drop_geometry() %>%
                        dplyr::select(-total) %>%
                        dplyr::select_if(is.numeric), type = "pie",
                      colorPalette = unname(la_palette),
                      width =  8 + ((zoom_level() * 20) * (df$total / max(df$total))),
                      legend = TRUE, legendPosition = "bottomright", layerId = "minicharts") %>%
        addLayersControl(baseGroups = c("minicharts", "grid"), overlayGroups = c("background"))
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
      # shinyjs::click("submit")
      
    })
    
    # return(list(newwkt = newwkt))
    
  })
}
