pieMapTimeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("pie_map")),
    dygraphOutput(ns("plot_by_time"))
  )
}

pieMapTimeSeriesServer <- function(id, category_var, sql_query,centroid) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
    
    # Leaflet map output
    output$pie_map <- renderLeaflet({
      req(data_pie_map())
      req(centroid)
      df <- data_pie_map()
      lat_centroid <- st_coordinates(centroid())[2]
      lon_centroid <- st_coordinates(centroid())[1]
      la_palette <- getPalette(category_var)
      
      
      df <-  leaflet() %>%
        addProviderTiles("Esri.NatGeoWorldMap", group = "background") %>%
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
                      width = (60*df$total/max(df$total))+20,
                      legend = TRUE, legendPosition = "bottomright") %>%
        addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
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
      
      # Plot the dygraph with one line per species
      g1 <- dygraph(tuna_catches_timeSeries) %>%
        dyOptions(fillGraph = TRUE) %>%
        dyGroup(colnames(tuna_catches_timeSeries)) %>%
        dyRangeSelector()
      
      g1
    })
  })
}
