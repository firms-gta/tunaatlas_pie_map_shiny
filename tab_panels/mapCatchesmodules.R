mapCatchesUI <- function(id) {
  ns <- NS(id)
  tagList(
  leafletOutput(ns("map_total_catch"), width = "100%", height = "400px")%>% withSpinner(),
  actionButton(ns("submit_draw_total"), "Update wkt from drawing",
               class = "btn-primary",
               style = "position: absolute; top: 100px; right: 20px; z-index: 400; font-size: 0.8em; padding: 5px 10px;"))
  # submitWktUI(id = ns("submit_wkt")))
}

# Module Server
mapCatchesServer <- function(id, data, submitTrigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    sum_all <- reactive({
      req(data())
      df <- data()
      
      df %>%
        dplyr::group_by(geom_wkt) %>%
        dplyr::summarise(measurement_value = sum(measurement_value)) %>%
        as.data.frame() %>%
        st_as_sf()
    })
    
    output$map_total_catch <- renderLeaflet({
      req(sum_all())
      flog.info("Rendering total catch map")
      a <- sum_all()
      flog.info("Sum all data: %s", head(a))
      
      qpal <- colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
      
      leaflet() %>%
        addProviderTiles("Esri.NatGeoWorldMap") %>%
        clearBounds() %>%
        addPolygons(data = a,
                    label = ~measurement_value,
                    popup = ~paste0("Total catches for the selected criteria in this square of the grid: ", round(measurement_value), " tons (t) et des brouettes"),
                    fillColor = ~qpal(measurement_value),
                    fill = TRUE,
                    fillOpacity = 0.8,
                    smoothFactor = 0.5, weight = 2) %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        ) %>%
        addLayersControl(
          overlayGroups = c("draw"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend("bottomright", pal = qpal, values = a$measurement_value,
                  title = "Quantile of the grid for the total catches",
                  labFormat = labelFormat(prefix = "MT "),
                  opacity = 1)
    })
    
    observeEvent(input$submit_draw_total, {
      flog.info("User requested to change spatial coverage")
      showModal(modalDialog(
        title = "Changing spatial coverage",
        "Attention, you are about to change the geographic coverage of the filter. Are you sure?",
        footer = tagList(
          modalButton("No"),
          actionButton(ns("yes_button"), "Yes")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$yes_button, {
      flog.info("User confirmed changing spatial coverage")
      req(input$map_total_catch_draw_new_feature$geometry)
      req(input$map_total_catch_draw_stop)
      
      geojson <- input$map_total_catch_draw_new_feature$geometry
      geojson_text <- toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
      sf_obj <- geojsonsf::geojson_sf(geojson_text)
      
      wkt_val <- st_as_text(sf_obj$geometry)
      wkt(wkt_val)
      submitTrigger(TRUE)
      removeModal()
      flog.info("Spatial coverage changed")
    })
    
    
  })
}

