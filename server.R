server <- function(input, output, session) {
  
  addResourcePath("www", here::here("www"))
  serveRmdContents("rmd_docs", nav_bar_menu_html) # to create rmd tabpanels
  
  shinyjs::onclick("fishing_fleet_toggle", {
    shinyjs::toggle("fishing_fleet_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  
  shinyjs::onclick("gear_type_toggle", {
    shinyjs::toggle("gear_type_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  
  submitTrigger <- reactiveVal(FALSE)
  firstSubmit <- reactiveVal(TRUE)
  
  selected_dataset <- reactiveVal()
  selected_gridtype <- reactiveVal()
  selected_measurement_unit <- reactiveVal()
  initial_data <- reactiveVal()
  data_for_filters <- reactiveVal()
  data_loaded <- reactiveVal(FALSE)
  show <- reactiveVal(FALSE)
  
  output$select_dataset <- renderUI({
    datasets <- filters_combinations %>% dplyr::select(dataset) %>% dplyr::distinct()
    selectizeInput('select_dataset', 'Select the Dataset', choices = datasets$dataset, selected = default_dataset)
  })
  
  output$select_gridtype <- renderUI({
    req(input$select_dataset)
    gridtypes <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% dplyr::select(gridtype) %>% dplyr::distinct()
    selectizeInput('select_gridtype', 'Select the Grid Type', choices = gridtypes$gridtype, selected = default_gridtype)
  })
  
  output$select_measurement_unit <- renderUI({
    req(input$select_dataset)
    measurement_units <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% dplyr::select(measurement_unit) %>% dplyr::distinct()
    selectizeInput('select_measurement_unit', 'Select the Measurement unit', choices = measurement_units$measurement_unit, selected = default_measurement_unit)
  })
  
  observeEvent(show(), {
    if (show()) {
      shinyjs::hide("loading_page")
      shinyjs::show("main_content")
      updateNavbarPage(session, "main", selected = "mainpanel")
    }
  })
  
  shinyjs::delay(1000, { shinyjs::click("submitDataset") })
  
  observeEvent(input$submitDataset, {
    flog.info("Submit dataset clicked")
    
    #First submitting outside
    if(firstSubmit()){
      flog.info("First submit")
      
      flog.info("All initialization files already exist. Loading from files.")
      flog.info("loading inital data")
      
      default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
      flog.info("Data sf loaded")
      
      if(exists("default_dataset_preloaded")){
        initial_data(default_dataset_preloaded)
        default_dataset_preloaded_without_geom <- default_dataset_preloaded
        default_dataset_preloaded_without_geom$geom <- NULL
        default_dataset_preloaded_without_geom$geom_wkt <- NULL
        data_for_filters(default_dataset_preloaded_without_geom)
        show(TRUE)
        observeEvent(TRUE, {
          show(FALSE)
          shinyjs::show("loading_page")
          
        })
        
        
      }
    } else {
      
      shinyjs::show(selector = "#side_panel")
      
      showNotification("Loading big dataset please wait", type = "message", duration = NULL, id="loadingbigdata")
      
      selected_dataset(input$select_dataset)
      selected_gridtype(input$select_gridtype)
      selected_measurement_unit(input$select_measurement_unit)
      
      query <- glue::glue_sql(
        "SELECT gridtype, geom_id, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, geom, ST_AsText(geom) AS geom_wkt, year, month FROM public.shinycatch 
      WHERE dataset = {selected_dataset()} AND gridtype = {selected_gridtype()} AND measurement_unit = {selected_measurement_unit()}
      GROUP BY gridtype, species, fishing_fleet, geom_id, geom_wkt, geom, year, month, gear_type, fishing_mode, measurement_unit",
        .con = pool
      )
      
      tryCatch({
        data <- dbGetQuery(pool, query)
        flog.info("Data loaded from database")
        flog.info("Class data: %s", class(data))  
        
        data_sf <- as.data.frame(st_as_sf(data, wkt = "geom_wkt", crs = 4326))
        flog.info("beginning intersection")
        flog.info("Class data_sf: %s", class(data_sf)) 
        
        initial_data(data_sf)
        flog.info("Class initial_data: %s", class(initial_data())) 
        initial_data_without_geom <- data_sf
        initial_data_without_geom$geom <- NULL
        data_for_filters(initial_data_without_geom)
        
        flog.info("Dataset created. You can now filter it.")
        
      }, error = function(e) {
        showNotification("Error retrieving data: Please check your selections and try again.", type = "error")
        flog.error("Error executing query: %s", e$message)
      })
      
      data_loaded(TRUE)
      
      
      flog.info("Clicking on submit")
      
      shinyjs::delay(1, { shinyjs::click("submit") })
      
      showNotification("Dataframe loaded", type = "message", id="loadingbigdata")
      
    }
    
  })
  
  observe({
    req(data_for_filters())
    data_for_filters <- data_for_filters()
    flog.info("Initialising species")
    species_data <- data_for_filters %>% dplyr::select(species) %>% dplyr::distinct()
    flog.info("Species data after distinct: %s", head(species_data))
    
    output$select_species <- renderUI({
      selectizeInput('select_species', 'Select Species', choices = species_data$species, multiple = TRUE, selected = species_data$species[1])
    })
    flog.info("Initialising gears")
    gear_type <- data_for_filters %>% dplyr::select(gear_type) %>% dplyr::distinct()
    flog.info("Gear type data: %s", head(gear_type))
    
    output$select_gear_type <- renderUI({
      selectizeInput('select_gear_type', 'Select Gear', choices = gear_type$gear_type, multiple = TRUE, selected = gear_type$gear_type)
    })
    flog.info("Initialising fleets")
    fleets <- data_for_filters %>% dplyr::select(fishing_fleet) %>% dplyr::distinct()
    flog.info("Fishing fleet data: %s", head(fleets))
    
    output$select_fishing_fleet <- renderUI({
      selectizeInput('select_fishing_fleet', 'Select the Fishing Fleet', choices = fleets$fishing_fleet, multiple = TRUE, selected = fleets$fishing_fleet)
    })
    flog.info("Initialising fishing modes")
    fishing_modes <- data_for_filters %>% dplyr::select(fishing_mode) %>% dplyr::distinct()
    flog.info("Fishing mode data: %s", head(fishing_modes))
    
    output$select_fishing_mode <- renderUI({
      selectizeInput('select_fishing_mode', 'Select the Fishing mode', choices = fishing_modes$fishing_mode, multiple = TRUE, selected = fishing_modes$fishing_mode)
    })
    flog.info("Fishing mode selected")
    
    flog.info("Initialising years")
    
  })
  
  output$year_input <- renderUI({
    req(data_for_filters())
    
    years_for_filtering <- data_for_filters() %>%
      dplyr::summarise(min_year = min(year), max_year = max(year))
    flog.info("Year range: %d - %d", years_for_filtering$min_year, years_for_filtering$max_year)
    
    if (years_for_filtering$min_year == years_for_filtering$max_year) {
      selectInput("years", "Year", choices = years_for_filtering$min_year, selected = years_for_filtering$min_year)
    } else if (input$toggle_year) {
      flog.info("Choosing discrete(s) year")
      selectizeInput("years", "Choose discrete(s) year", choices = years_for_filtering$min_year:years_for_filtering$max_year, selected = round(mean(years_for_filtering$min_year:years_for_filtering$max_year)), multiple = TRUE)
    } else {
      flog.info("Choosing a period")
      sliderInput("years", "Choose a period", min = years_for_filtering$min_year, max = years_for_filtering$max_year, value = c(years_for_filtering$min_year, years_for_filtering$max_year), step = 1, round = TRUE)
    }
  })
  
  observeEvent(input$all_species, {
    flog.info("Select all species")
    req(data_for_filters())
    species <- data_for_filters() %>% dplyr::select(species) %>% dplyr::distinct() %>% pull(species)
    flog.info("Select all species: %s", paste(species, collapse = ", "))
    updateSelectInput(session, "select_species", selected = species)
  })
  
  observeEvent(input$major_tunas, {
    flog.info("Select major tunas")
    req(data_for_filters())
    species <- data_for_filters() %>% dplyr::filter(species %in% c("YFT", "SKJ", "ALB", "BET", "SBF")) %>% dplyr::select(species) %>% dplyr::distinct() %>% pull(species)
    updateSelectInput(session, "select_species", selected = species)
  })
  
  observeEvent(input$all_fishing_fleet, {
    flog.info("Select all fishing fleets")
    req(data_for_filters())
    fleets <- data_for_filters() %>% dplyr::select(fishing_fleet) %>% dplyr::distinct() %>% pull(fishing_fleet)
    updateSelectInput(session, "select_fishing_fleet", selected = fleets)
  })
  
  observeEvent(input$all_gear_type, {
    flog.info("Select all gear types")
    req(data_for_filters())
    gear_type <- data_for_filters() %>% dplyr::select(gear_type) %>% dplyr::distinct() %>% pull(gear_type)
    updateSelectInput(session, "select_gear_type", selected = gear_type)
  })
  
  observeEvent(input$all_fishing_mode, {
    flog.info("Select all fishing modes")
    req(data_for_filters())
    fishing_modes <- data_for_filters() %>% dplyr::select(fishing_mode) %>% dplyr::distinct() %>% pull(fishing_mode)
    updateSelectInput(session, "select_fishing_mode", selected = fishing_modes)
  })
  
  observeEvent(input$resetFilters, {
    data_loaded(TRUE)  
  })
  
  catches_by_variable_moduleServer("catches_by_variable_month", data_without_geom)
  
  final_filtered_data <- eventReactive(input$submit, {
    flog.info("Submit button cliqued")
    req(initial_data())
    req(data_for_filters())
    if(!firstSubmit()){
      showNotification("Filtering of the data", type = "message", duration = NULL, id = "filtrage")}
    
    flog.info("Filtrage")
    
    final_filtered_data <- initial_data()
    
    if (!is.null(input$select_species)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(species %in% input$select_species)
    }
    
    if (!is.null(input$select_gear_type)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(gear_type %in% input$select_gear_type)
    }
    
    if (!is.null(input$select_fishing_fleet)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(fishing_fleet %in% input$select_fishing_fleet)
    }
    
    if (!is.null(input$select_gridtype)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(gridtype %in% input$select_gridtype)
    }
    
    if (!is.null(input$select_fishing_mode)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(fishing_mode %in% input$select_fishing_mode)
    }
    
    if (!is.null(input$toggle_year) && !is.null(input$years)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(year %in% (if (input$toggle_year) input$years else seq(input$years[1], input$years[2])))
    }
    if(!firstSubmit()){
      showNotification("Filtering finished", type = "message", id = "filtrage")
    }
    flog.info("Filtrage terminé")
    flog.info("Nrow final_filtered_data %s", nrow(final_filtered_data))
    firstSubmit(FALSE)
    show(TRUE)
    # data_loaded(FALSE)
    
    final_filtered_data
  }, ignoreNULL = FALSE)
  
  # this one is not working
  data_without_geom <- reactive({
    req(final_filtered_data())
    
    flog.info("Removing geometry column from data")
    
    data_without_geom <- as.data.frame(final_filtered_data())
    
    data_without_geom$geom_wkt <- NULL
    
    if("geom"%in% colnames(data_without_geom)){
      data_without_geom <- data_without_geom %>% dplyr::select(-geom)
    }
    
    flog.info("Data without geometry: %s", head(data_without_geom))
    data_without_geom
  })
  
  centroid <- reactive({
    flog.info("Calculating centroid")
    bbox <- st_as_sf(final_filtered_data()) %>% 
      st_bbox()  # Get the bounding box
    
    # Calculate the center of the bounding box
    center_lon <- (bbox["xmin"] + bbox["xmax"]) / 2
    center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
    
    # Create a POINT geometry for the centroid
    result <- st_point(c(center_lon, center_lat)) %>% 
      st_sfc(crs = st_crs(final_filtered_data()))  # Ensure the same CRS
    
    flog.info("Centroid: %s", st_as_text(result))
    result
  })
  
  observeEvent(input$resetWkt, {
    showModal(modalDialog(
      title = "Changing spatial coverage",
      "Attention, you are about to change the geographic coverage of the filter. Are you sure?",
      footer = tagList(
        modalButton("No"),
        actionButton("yes_button", "Yes")
      ),
      easyClose = TRUE,
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$yes_button, {
    wkt(global_wkt)
    submitTrigger(TRUE)
    removeModal()
  })
  
  ############################################################# OUTPUTS ############################################################# 
  
  output$sql_query_init <- renderText({ 
    paste(sql_query_init)
  })
  
  output$DT <- renderDT({
    req(data_without_geom())
    data_without_geom()
  }) 
  
  data_time_serie_species <- reactive({
    req(data_without_geom())
    flog.info("Creating time series data for species")
    result <- data_without_geom() %>%
      dplyr::group_by(species, year) %>%
      dplyr::summarise(measurement_value = sum(measurement_value)) %>%
      tidyr::spread(species, measurement_value, fill = 0)
    flog.info("Time series species data: %s", head(result))
    result
  })
  
  output$Data_wide_species <- renderDT({
    req(data_time_serie_species())
    data_time_serie_species()
  })
  
  
  
  output$plot11 <- renderImage({
    df_i11_filtered <- as(final_filtered_data(), "Spatial")
    
    i11 <- Atlas_i11_CatchesByCountry(df=df_i11_filtered,
                                      geomIdAttributeName="geom_id",
                                      countryAttributeName="fishing_fleet",
                                      speciesAttributeName="species",
                                      valueAttributeName="measurement_value",
                                      withSparql=FALSE)
    
    i11
    png(i11, width = 400, height = 300)
    dev.off()
    
    list(src = i11,
         contentType = 'image/png',
         width = 1600,
         height = 1200,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  categoryGlobalPieChartServer("fishing_fleet_chart", "fishing_fleet", data_without_geom)
  categoryGlobalPieChartServer("species_chart", "species", data_without_geom)
  categoryGlobalPieChartServer("gear_type_chart", "gear_type", data_without_geom)
  categoryGlobalPieChartServer("fishing_mode_chart", "fishing_mode", data_without_geom)
  
  pieMapTimeSeriesServer("species_module", category_var = "species", data = final_filtered_data, centroid = centroid, submitTrigger = submitTrigger)
  pieMapTimeSeriesServer("fishing_fleet_module", category_var = "fishing_fleet", data = final_filtered_data, centroid = centroid, submitTrigger = submitTrigger)
  pieMapTimeSeriesServer("gear_type_module", category_var = "gear_type", data = final_filtered_data, centroid = centroid, submitTrigger = submitTrigger)
  pieMapTimeSeriesServer("fishing_mode_module", category_var = "fishing_mode", data = final_filtered_data, centroid = centroid, submitTrigger = submitTrigger)
  
  TimeSeriesbyDimensionServer("species_timeseries", category_var = "species", data = data_without_geom)
  TimeSeriesbyDimensionServer("fishing_fleet_timeseries", category_var = "fishing_fleet", data = data_without_geom)
  TimeSeriesbyDimensionServer("gear_type_timeseries", category_var = "gear_type", data = data_without_geom)
  TimeSeriesbyDimensionServer("fishing_mode_timeseries", category_var = "fishing_mode", data = data_without_geom)
  
  mapCatchesServer("total_catch", data = final_filtered_data, submitTrigger)
  plotTotalCatchesServer("catch_by_year", data = data_without_geom)
  
  observeEvent(input$change_dataset, {
    updateNavbarPage(session, "main", selected = "datasetchoicevalue")
  })
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("GTA_species_dataset_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      csv_tuna <- final_filtered_data()
      write.csv(csv_tuna, file)
    }
  )
  
  output$downloadCsvtableCWP <- downloadHandler(
    filename = function() {
      paste("GTA_CWP_dataset_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      CWP_dataset <- final_filtered_data()
      write.csv(CWP_dataset, file)
    }
  )
  
  output$head_table_init <- renderDataTable({
    req(final_filtered_data())
    head(final_filtered_data())
  })
  
  onStop(function() {
    poolClose(pool)
  })
}
