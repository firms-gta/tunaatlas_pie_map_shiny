mapCatchesUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(
      inputId = ns("mode"),
      label   = "Map mode:",
      choices = c("Global footprint (light plot)" = "light",
                  "Full choropleth (leaflet, for reduced datasets)"    = "full"),
      selected = "light",
      inline   = TRUE
    ),
    # Light = plot statique; Full = leaflet
    conditionalPanel(
      condition = sprintf("input['%s'] == 'light'", ns("mode")),
      withSpinner(plotOutput(ns("map_light_plot"), height = "380px"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'full'", ns("mode")),
      div(
        withSpinner(leafletOutput(ns("map_total_catch"), height = "380px")),
        actionButton(
          ns("submit_draw_total"), "Update wkt from drawing",
          class = "btn-primary",
          style = "position: absolute; top: 100px; right: 20px; z-index: 400;
               font-size: 0.8em; padding: 5px 10px;"
        )
      )
    )
    
  )
}


# Module Server
mapCatchesServer <- function(id, data, geom_sf, enabled = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    newwkt <- reactiveVal()
    world_sf <- reactiveVal(NULL)
    observeEvent(TRUE, {
      if (is.null(world_sf())) {
        suppressMessages({
          world_sf(rnaturalearth::ne_countries(scale = "small", returnclass = "sf"))
        })
      }
    }, once = TRUE, ignoreInit = FALSE)
    
    # --- Prépare la grille en WGS84 pour affichage/leaflet
    geom_ll  <- sf::st_transform(geom_sf, 4326)
    
    # --- Empreinte: cellules présentes (WGS84)
    present_cells <- reactive({
      req(isTRUE(enabled()), data())
      ids <- unique(data()$geographic_identifier)
      fp  <- dplyr::semi_join(geom_ll, tibble::tibble(geographic_identifier = ids),
                              by = "geographic_identifier")
      if (nrow(fp) == 0) return(NULL)
      fp
    })
    
    # --- Empreinte dissoute (calcul robuste en mètres, sortie en WGS84)
    footprint_dissolved <- reactive({
      fp_ll <- present_cells(); if (is.null(fp_ll)) return(NULL)
      # reprojeter en projection métrique pour union/simplify
      fp_m <- sf::st_transform(fp_ll, 3857)
      if (any(!sf::st_is_valid(fp_m))) fp_m <- sf::st_make_valid(fp_m)
      # union + simplification (tolérance ~ 20 km, ajustable)
      u <- sf::st_union(fp_m)
      if (length(u) == 0 || all(sf::st_is_empty(u))) return(NULL)
      u <- sf::st_simplify(u, dTolerance = 20000)  # 20 km
      sf::st_transform(sf::st_as_sf(tibble::tibble(geometry = u)), 4326)
    })
    
    # -------------------------
    # LIGHT: PLOT STATIQUE
    # -------------------------
    output$map_light_plot <- renderPlot({
      req(isTRUE(enabled()), input$mode == "light")
      fp <- footprint_dissolved(); req(!is.null(fp))
      w  <- world_sf();           req(!is.null(w))
      
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = w, fill = "grey95", color = "grey80", linewidth = 0.2) +
        ggplot2::geom_sf(data = fp, fill = "steelblue", color = NA, alpha = 0.6) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
          axis.text  = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.margin = grid::unit(c(2,2,2,2), "mm")
        )
    })
    
    # -------------------------
    # FULL: LEAFLET CHOROPLETH
    # -------------------------
    # --- couche FULL (ne s'évalue que si mode == "full")
    full_layer <- reactive({
      req(isTRUE(enabled()))
      req(identical(input$mode, "full"))   # <-- rien ne se calcule si "light"
      req(data())
      
      # agrégation
      df <- data() |>
        dplyr::group_by(geographic_identifier) |>
        dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
      
      # géométrie en WGS84 pour leaflet
      geom_ll <- sf::st_transform(geom_sf, 4326)
      
      # jointure
      a <- dplyr::inner_join(geom_ll, df, by = "geographic_identifier")
      shiny::validate(need(nrow(a) > 0, "No data to display for 'full' mode."))
      
      a  # <-- on RETOURNE l'objet sf
    })
    
    # --- rendu Leaflet (uniquement si mode == "full")
    # output$map_total_catch <- leaflet::renderLeaflet({
    #   req(isTRUE(enabled()))
    #   req(identical(input$mode, "full"))   # <-- ne rend rien si "light"
    #   
    #   a <- full_layer()                    # a est un sf non-vide
    #   qpal <- leaflet::colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
    #   bb <- sf::st_bbox(a)
    #   
    #   leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE, minZoom = 1)) |>
    #     leaflet::addProviderTiles("CartoDB.PositronNoLabels") |>
    #     addDrawToolbar(
    #       targetGroup = "draw",
    #       editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
    #     ) |>
    #     leaflet::addPolygons(
    #       data = a,
    #       label = ~format(round(measurement_value), big.mark = " "),
    #       fillColor = ~qpal(measurement_value),
    #       fillOpacity = 0.8, weight = 1, color = "#444444"
    #     ) |>
    #     leaflet::addLegend(
    #       position = "bottomright", pal = qpal, values = a$measurement_value,
    #       title = "Total catches (quantiles)", opacity = 1
    #     ) |>
    #     leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    # }) 
    
    output$map_total_catch <- renderLeaflet({
      req(isTRUE(enabled()))
      flog.info("🔍 Chargement de la carte")
      req(identical(input$mode, "full"))   # <-- ne rend rien si "light"
      req(full_layer())  # Attendre que sum_all soit calculé
      
      a <- full_layer()                    # a est un sf non-vide
      qpal <- leaflet::colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
      # # ✅ Utilise firstSubmit() pour savoir si on charge la carte pré-enregistrée
      # if (firstSubmit) {
      #   flog.info("firstSubmit est TRUE, chargement de la carte pré-enregistrée")
      #   file.remove("data/test.qs")
      #   return(qs::qread("data/map_init.qs"))
      # }
      
      
      
      flog.info("🗺 Rendering total catch map")
      # a <- sum_all()
      # a <- st_simplify(a, dTolerance = 0.01)  
      
      qpal <- colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
      map <- leaflet() %>%
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
      
      flog.info("✅ Carte terminée")
      map
    })
    
    # (optionnel) garder ceci si tu as aussi un output 'map_light_plot'
    outputOptions(output, "map_light_plot",  suspendWhenHidden = FALSE)
    outputOptions(output, "map_total_catch", suspendWhenHidden = FALSE)
    
    
    
    # observe({
    #   req(enabled())
    #   m <- leafletProxy("map_total_catch", session) |>
    #     clearShapes() |>
    #     clearControls()
    #   
    #   if (identical(input$mode, "full")) {
    #     a <- full_layer()
    #     qpal <- colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
    #     m |>
    #       addPolygons(
    #         data = a,
    #         label = ~format(round(measurement_value), big.mark = " "),
    #         fillColor = ~qpal(measurement_value),
    #         fillOpacity = 0.8, weight = 1, color = "#444444"
    #       ) |>
    #       addLegend(position = "bottomright", pal = qpal, values = a$measurement_value,
    #                 title = "Total catches (quantiles)", opacity = 1)
    #   } else {
    #     # map_light_plot
    #   }
    # })
    
    # outputOptions(output, "map_light_plot", suspendWhenHidden = FALSE)
    # outputOptions(output, "map_total_catch",       suspendWhenHidden = FALSE)
  
    # outputOptions(output, "map_total_catch", suspendWhenHidden = FALSE) # hyper important empêche le rechargement
    
    observeEvent(input$submit_draw_total, {
      req(enabled())
      req(input$submit_draw_total)
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
      req(enabled())
        req(input$yes_button_total_map)
        isolate({
          req(input$map_total_catch_draw_new_feature$geometry)
          req(input$map_total_catch_draw_stop)
        })
        
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
      shinyjs::click("submit")
      
    })
    
    return(list(newwkt = newwkt))
    
    
  })
}

