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
mapCatchesServer <- function(id, data, submitTrigger, geom) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    newwkt <- reactiveVal()
    sum_all <- reactive({
      req(data())
      df <- data()
      geom <- geom()
      df <- st_as_sf(as.data.frame(df %>%
        dplyr::group_by(geographic_identifier) %>%
        dplyr::summarise(measurement_value = sum(measurement_value)) %>%
        # as.data.frame() %>%
        # st_as_sf() %>% 
        dplyr::left_join(st_as_sf(geom))))
      df
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
        addPolygons(
          data = a,
          label = ~measurement_value,
          popup = ~paste0("Total value for the selected criteria in this square of the grid: ", round(measurement_value)),
          fillColor = ~qpal(measurement_value),
          fill = TRUE,
          fillOpacity = 0.8,
          smoothFactor = 0.5, weight = 2
        ) %>%
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
        addLegend(
          position = "bottomright", 
          pal = qpal, 
          values = a$measurement_value,
          title = "Quantile of the grid for the total catches",
          labFormat = labelFormat(prefix = "MT "),
          opacity = 1
        ) %>%
        addControl(
          html = HTML("
      <div style='background-color: rgba(255, 255, 255, 0.8); padding: 5px; border-radius: 5px;'>
        <p style='font-size:12px; color:grey; margin:0;'>
          <b>Caution:</b> Selecting multiple units in the filters will sum their values,<br>
          which may lead to inaccurate results.
        </p>
      </div>
    "),
          position = "bottomleft",  
          className = "legend-popup"
        )
    })
    
    observeEvent(input$submit_draw_total, {
      
      flog.info("Submitting draw")
      if (!is.null(input$map_total_catch_draw_new_feature) && 
          !is.null(input$map_total_catch_draw_new_feature$geometry) && 
          length(input$map_total_catch_draw_new_feature$geometry$coordinates) > 0) {
        
        showModal(modalDialog(
          title = "Changing spatial coverage",
          "Attention, you are about to change the geographic coverage of the filter, it can take some time. Are you sure?",
          footer = tagList(
            modalButton("No"),
            actionButton(ns("yes_button_total_map"), "Yes")  # Ensure ns is used
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
    
    observeEvent(input$yes_button_total_map, {
      flog.info("Yes button clicked changing the wkt")
      req(input$map_total_catch_draw_new_feature$geometry)
      req(input$map_total_catch_draw_stop)
      flog.info("Draw shape exists")
      
      geojson <- input$map_total_catch_draw_new_feature$geometry
      # Convert GeoJSON to sf object
      flog.info("geojson ok")
      
      geojson_text <- toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)
      sf_obj <- geojsonsf::geojson_sf(geojson_text)
      flog.info("sf objok ")

            # Convert to WKT
      wkt_val <- st_as_text(sf_obj$geometry)
        # wkt(wkt_val)  # Mettre à jour la valeur réactive WKT
        flog.info("WKT mis à jour: %s", wkt_val)
        
        # submitTrigger(TRUE) 
      # wkt(wkt_val)  # Update the reactive value with the WKT representation
      flog.info("wkt")
      removeModal()
      # flog.info("submittrigger")
      flog.info("WKT mis à jour: %s", wkt_val)
      newwkt(wkt_val)
      # shinyjs::click("submit")
      
    })
    
    return(list(newwkt = newwkt))
    
    
  })
}

