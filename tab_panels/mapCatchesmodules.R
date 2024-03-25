# Module UI
mapCatchesUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map_total_catch"), width = "100%", height = "400px")
}

# Module Server
mapCatchesServer <- function(id, sum_all) {
  moduleServer(id, function(input, output, session) {
    output$map_total_catch <- renderLeaflet({
      a <- sum_all()
      qpal <- colorQuantile(rev(viridis::viridis(10)),a$measurement_value, n=10)
      my_map <- leaflet() %>% 
        addProviderTiles("Esri.NatGeoWorldMap") %>% 
        clearBounds() %>%
        addPolygons(data = a,
                    label = ~measurement_value,
                    popup = ~paste0("Total catches for the selected criteria in this square of the grid: ", round(measurement_value), " tons (t) et des brouettes"),
                    fillColor = ~qpal(measurement_value),
                    fill = TRUE,
                    fillOpacity = 0.8,
                    smoothFactor = 0.5) %>% 
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
        leaflet::addLegend("bottomright", pal = qpal, values = a$measurement_value,
                           title = "Quantile of the grid for the total catches",
                           labFormat = labelFormat(prefix = "MT "),
                           opacity = 1
        )
      return(my_map)
    })
    
    })
}
