pieMapTimeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "height: 400px;",
      leafletOutput(ns("pie_map")),
      # submitWktUI(id = ns("submit_wkt_pie_map"))
      actionButton(ns("submit_draw"), "Update wkt from drawing",
                   class = "btn-primary",
                   style = "position: absolute; top: 100px; right: 20px; z-index: 400; font-size: 0.8em; padding: 5px 10px;")
    ),
    tags$br(),
    dygraphOutput(ns("plot_by_time"), height = "300px") 
  )
}


pieMapTimeSeriesServer <- function(id, category_var, sql_query,centroid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    zoom_level <- reactiveVal(1)
    target_var <- getTarget(category_var)
    
    # Adjust the reactive expressions to use the dynamic category_var
    data_pie_map <- reactive({
      req(sql_query())
      query <- paste0("SELECT ", category_var, ", geom, sum(measurement_value) AS measurement_value FROM(", sql_query(), ") AS foo GROUP BY ", category_var, ", geom")
      df <- st_read(pool, query = query) %>%
        tidyr::spread(key = !!sym(category_var), value = measurement_value, fill = 0) %>%
        dplyr::mutate(total = rowSums(across(any_of(target_var[[category_var]]))))
      df
    })
    la_palette <- reactive({
      la_palette <- getPalette(category_var)
      la_palette = la_palette[names(la_palette) %in% colnames(data_pie_map())]
      
      }) 

    
    # Leaflet map output
    output$pie_map <- renderLeaflet({
      req(data_pie_map(), zoom_level())  # Ensure data and zoom level are available
      req(centroid)
      df <- data_pie_map()
      lat_centroid <- st_coordinates(centroid())[2]
      lon_centroid <- st_coordinates(centroid())[1]
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
      
      // Trigger the zoom level input update on map initialization
      Shiny.setInputValue('%smap_zoom_level', map.getZoom(), {priority: 'event'});
    }", session$ns(""), session$ns("")
          )
        )%>% 
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
                      chartdata = dplyr::select(df,-total) %>% st_drop_geometry(),type = "pie",
                      colorPalette = unname(la_palette),
                      width =  8+((zoom_level()*100)*(df$total/max(df$total))),#5+(zoom_level()^2+100*(df$total/max(df$total))), #((60 * df$total / max(c$total)) + 10) * (10 / zoom_level()),#(60*df$total/max(df$total))+20,
                      legend = TRUE, legendPosition = "bottomright", layerId = "minicharts") %>%
        addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
    })
    
    
    observeEvent(input$map_zoom_level, {
      # Make sure data_pie_map() is evaluated once at the beginning to avoid multiple evaluations.
      df <- data_pie_map()
      la_palette <- la_palette()
      
      
      # Assuming input$map_zoom_level is a number. Adjust the calculation as needed.
      new_width <- 8 + (input$map_zoom_level * 20) * (df$total / max(df$total))
      
      # Update minicharts directly within this observer
      leafletProxy("pie_map", data = df) %>%
        clearGroup("minicharts") %>%  # Clear existing minicharts
        addMinicharts(lng = st_coordinates(st_centroid(df, crs = 4326))[, "X"],
                      lat = st_coordinates(st_centroid(df, crs = 4326))[, "Y"],
                      maxValues = max(df$total),
                      chartdata = dplyr::select(df, -total) %>% st_drop_geometry(),
                      type = "pie",
                      colorPalette = unname(la_palette), transitionTime = 50,
                      width = new_width,  # Apply the dynamically calculated width
                      legend = TRUE, legendPosition = "bottomright")
    })
    
    # wktdraw <- submitWktServer(id = session$ns("submit_wkt_pie_map"))
    # wkt(wktdraw)
    ## same without using moduls
    observeEvent(input$submit_draw, {
      req(input$pie_map_draw_new_feature$geometry)
      req(input$pie_map_draw_stop)
      geojson <- input$pie_map_draw_new_feature$geometry
      # Convert GeoJSON to sf object
      geojson_text <- toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
      sf_obj <- geojsonsf::geojson_sf(geojson_text)


      # Convert to WKT
      wkt_val <- st_as_text(sf_obj$geometry)
      wkt(wkt_val)  # Update the reactive value with the WKT representation
    })

    
    # Time series plot output
    data_time_series <- reactive({
      req(sql_query()) 
      query <- paste0("SELECT ", category_var, ", to_date(year::varchar(4),'YYYY') AS year, sum(measurement_value) AS measurement_value FROM(", sql_query(), ") AS foo GROUP BY ", category_var, ", year")
      df <- st_read(pool, query = query)
      df
    })
    
    output$plot_by_time <- renderDygraph({
      
      df_i1 <- data_time_series()
      df_wide <- df_i1 %>%
        tidyr::spread(key = category_var, value = measurement_value, fill = 0)
      
      # Convert to xts object
      tuna_catches_timeSeries <- xts(df_wide[-1], order.by = as.Date(df_wide$year))
      
      series_names <- colnames(tuna_catches_timeSeries)
      la_palette <- la_palette()
      series_colors <- la_palette[series_names]
      # Plot the dygraph with one line per species
      g1 <- dygraph(tuna_catches_timeSeries) %>%
        dyOptions(fillGraph = TRUE, colors = series_colors) %>%
        dyGroup(colnames(tuna_catches_timeSeries)) %>%
        dyRangeSelector()
      
      g1
    })
  })
}

