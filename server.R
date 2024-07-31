server <- function(input, output, session) {
  
  flog.info("Default dataset preloaded: %s", !is.null(default_dataset))
  flog.info("Variables to display: %s", paste(variable_to_display, collapse = ", "))
  
  
  # Initialize resource paths and modules
  addResourcePath("www", here::here("www"))
  serveRmdContents("rmd_docs", nav_bar_menu_html)
  
  # Handle UI elements with shinyjs
  shinyjs::onclick("fishing_fleet_toggle", {
    shinyjs::toggle("fishing_fleet_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  
  shinyjs::onclick("gear_type_toggle", {
    shinyjs::toggle("gear_type_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  # Handle UI elements with shinyjs
  shinyjs::onclick("fishing_mode_toggle", {
    shinyjs::toggle("fishing_mode_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  
  shinyjs::onclick("species_toggle", {
    shinyjs::toggle("species_panel")
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");')
  })
  # Initialize reactive values
  submitTrigger <- reactiveVal(FALSE)
  firstSubmit <- reactiveVal(TRUE)
  selected_dataset <- reactiveVal()
  selected_gridtype <- reactiveVal()
  selected_measurement_unit <- reactiveVal()
  initial_data <- reactiveVal()
  data_for_filters <- reactiveVal()
  data_loaded <- reactiveVal(FALSE)
  show <- reactiveVal(FALSE)
  data_for_filters_trigger <- reactiveVal(0)
  
  # Render UI selectors
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
    selectizeInput('select_measurement_unit', 'Select the Measurement Unit', choices = measurement_units$measurement_unit, selected = default_measurement_unit)
  })
  
  # Show main content after loading
  observeEvent(show(), {
    if (show()) {
      shinyjs::hide("loading_page")
      shinyjs::show("main_content")
      updateNavbarPage(session, "main", selected = "generaloverview")
    }
  })
  
  dataset_choices <- dataset_choice_server("dataset_choice", filters_combinations, default_dataset, default_gridtype, default_measurement_unit)
  
  # Initial dataset submission
  observeEvent(dataset_choices$submit(), {
    flog.info("Submit dataset clicked")
    selected_dataset <- dataset_choices$selected_dataset()
    selected_gridtype <- dataset_choices$selected_gridtype()
    selected_measurement_unit <- dataset_choices$selected_measurement_unit()
    
    
    if (firstSubmit()) {
      flog.info("First submit")
      flog.info("All initialization files already exist. Loading from files.")
      flog.info("loading initial data")

      data <- load_initial_data(default_dataset, pool)
      
      flog.info("Data loaded")
      
      initial_data(data$initial_data)
      flog.info("Inital data loaded")
      
      data_for_filters(data$data_for_filters)
      flog.info("Filters loaded")
      
      show(TRUE)
      observeEvent(TRUE, {
        show(FALSE)
        shinyjs::show("loading_page")
      })
      
    } else {
      # shinyjs::hide("main_content")
      showNotification("Loading big dataset, please wait. Do not hesitate to use this laoding time to read the documentation on the app, the data and the metadata associated. ", type = "message", duration = NULL, id = "loadingbigdata")
      # shinyjs::show("loading_page")
      
      data <- load_query_data(selected_dataset, selected_gridtype, selected_measurement_unit, debug, pool)
      initial_data(data$initial_data)
      data_for_filters(data$data_for_filters)
      
      flog.info("Dataset created. You can now filter it.")
      shinyjs::hide("loading_page")
      
      showNotification("Dataframe loaded", type = "message", id = "loadingbigdata")
      shinyjs::show("main_content")
      wkt(global_wkt)
      
    }
    
    shinyjs::click("submit")
    shinyjs::delay(1000,{
    
    data_for_filters_trigger(data_for_filters_trigger() + 1)
      updateNavbarPage(session, "main", selected = "generaloverview")
    
  })
  })
  
  shinyjs::delay(1, { shinyjs::click("dataset_choice-submitDataset") })
  
  # Filtering the final data
  final_filtered_data <- eventReactive(input$submit, {

    flog.info("Submit button clicked")

    req(wkt())
    wkt <- wkt()
    req(initial_data())
    req(data_for_filters())

    if (!firstSubmit()) {
      showNotification("Filtering the data", type = "message", duration = NULL, id = "filtrage")
    }

    flog.info("Filtering")

    final_filtered_data <- initial_data()

    for (variable in variable_to_display) {
      select_input <- paste0("select_", variable)
      if (!is.null(input[[select_input]])) {
        final_filtered_data <- final_filtered_data %>%
          dplyr::filter(!!sym(variable) %in% input[[select_input]])
      }
    }

    if (!is.null(input$toggle_year) && !is.null(input$years)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(year %in% (if (input$toggle_year) input$years else seq(input$years[1], input$years[2])))
    }

    if(wkt != global_wkt){
      sf_wkt <- st_as_sfc(wkt, crs = 4326)
      final_filtered_data <- st_as_sf(final_filtered_data)
      final_filtered_data <- final_filtered_data[st_within(final_filtered_data, sf_wkt, sparse = FALSE), ]
    }

    if (!firstSubmit()) {
      showNotification("Filtering finished", type = "message", id = "filtrage")
    }


    flog.info("Filtering finished")
    firstSubmit(FALSE)
    flog.info("Nrow final_filtered_data %s", nrow(final_filtered_data))

    final_filtered_data
  }, ignoreNULL = FALSE)
  
  
  # Reactive function for data without geometry
  data_without_geom <- reactive({
    req(final_filtered_data())
    flog.info("Removing geometry column from data")
    data_without_geom <- as.data.frame(final_filtered_data())
    data_without_geom$geom_wkt <- NULL
    if ("geom" %in% colnames(data_without_geom)) {
      data_without_geom <- data_without_geom %>% dplyr::select(-geom)
    }
    flog.info("Data without geometry: %s", head(data_without_geom))
    data_without_geom
  })
  
  # Calculate the centroid of the map
  centroid <- reactive({
    flog.info("Calculating centroid")
    bbox <- st_as_sf(final_filtered_data()) %>% 
      st_bbox()
    center_lon <- (bbox["xmin"] + bbox["xmax"]) / 2
    center_lat <- (bbox["ymin"] + bbox["ymax"]) / 2
    result <- st_point(c(center_lon, center_lat)) %>% 
      st_sfc(crs = st_crs(final_filtered_data()))
    flog.info("Centroid: %s", st_as_text(result))
    result
  })
  
  # Update selectors and reactivity
  
  # ObserveEvent for the resetFilters button
  observeEvent(input$resetFilters, {
    flog.info("Resetting filters")
    data_for_filters_trigger(data_for_filters_trigger() + 1)
  })
  
  observeEvent(data_for_filters_trigger(), {
    req(data_for_filters())
    data_for_filters <- data_for_filters()
    
    lapply(variable_to_display, function(variable) {
      local({
        variable <- variable
        flog.info(paste("Initialising", variable))
        
        variable_data <- data_for_filters %>% dplyr::select(all_of(variable)) %>% dplyr::distinct()
        flog.info(paste(variable, "data after distinct:", paste(head(variable_data), collapse = ", ")))
        
        output[[paste0("select_", variable)]] <- renderUI({
          selectizeInput(paste0('select_', variable), 
                         paste('Select', gsub("_", " ", variable)), 
                         choices = variable_data[[variable]], 
                         multiple = TRUE, 
                         selected = variable_data[[variable]])
        })
        
        flog.info(paste(variable, "UI element initialized"))
      })
    })
  })
  
  
  output$year_input <- renderUI({
    req(data_for_filters())
    years_for_filtering <- data_for_filters() %>%
      dplyr::summarise(min_year = min(year, na.rm = TRUE), max_year = max(year, na.rm = TRUE))
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
  
    observeEvent(input$major_tunas, {
    flog.info("Select major tunas")
    req(data_for_filters())
    species <- data_for_filters() %>% dplyr::select(species)%>% dplyr::filter(species %in% c("YFT", "SKJ", "ALB", "BET", "SBF"))  %>% dplyr::distinct() %>% pull(species)
    updateSelectInput(session, "select_species", selected = species)
  })
    
    observeEvent(input$major_tunas_name, {
      flog.info("Select major tunas")
      req(data_for_filters())
      species_name <- data_for_filters()%>% dplyr::select(species_name) %>% dplyr::filter(species_name %in% c("Albacore", "Bigeye tuna", "Skipjack tuna", "Yellowfin tuna", "Southern bluefin tuna"))  %>% 
        dplyr::distinct() %>% pull(species_name)
      updateSelectInput(session, "select_species_name", selected = species_name)
    })
  
  lapply(variable_to_display, function(variable) {
    observeEvent(input[[paste0("all_", variable)]], {
      flog.info(paste("Select all", variable))
      req(data_for_filters())
      all_values <- data_for_filters() %>% dplyr::select(!!sym(variable)) %>% dplyr::distinct() %>% pull(!!sym(variable))
      updateSelectInput(session, paste0("select_", variable), selected = all_values)
    })
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
  
  
  observeEvent(submitTrigger(), {
    print("submittrigger")
    if(submitTrigger()) {
      print("submit")
      shinyjs::click("submit")  # Trigger the submit button click in the sidebar
      submitTrigger(FALSE)
    }
  })
  # Data and graphics outputs
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
  
  # Pie charts
  lapply(variable_to_display, function(variable) {
    local({ # to isolate each variable in its own environement, otherwise sometimes its only one of the variables that is displayed
      variable <- variable
      categoryGlobalPieChartServer(paste0(variable, "_chart"), variable, data_without_geom)
    })
  })
  
  # Map and time series
  lapply(variable_to_display, function(variable) {
    local({
      variable <- variable
      pieMapTimeSeriesServer(paste0(variable, "_module"), category_var = variable, data = final_filtered_data, centroid = centroid, submitTrigger = submitTrigger)
    })
  })
  
  # Time series by dimension
  lapply(variable_to_display, function(variable) {
    local({
      variable <- variable
      TimeSeriesbyDimensionServer(paste0(variable, "_timeseries"), category_var = variable, data = data_without_geom)
    })
  })
  
  # Global overview
  catches_by_variable_moduleServer("catches_by_variable_month", data_without_geom)
  mapCatchesServer("total_catch", data = final_filtered_data, submitTrigger)
  
  observeEvent(firstSubmit(), {
    if (!firstSubmit()) {
      flog.info("delay")
      catches_by_variable_moduleServer("catches_by_variable_month", data_without_geom)
      mapCatchesServer("total_catch", data = final_filtered_data, submitTrigger)
      shinyjs::delay(3000, {   
        show(TRUE)
        flog.info("delay finished")
      })
    }
  })
  
  plotTotalCatchesServer("catch_by_year", data = data_without_geom)
  
  observeEvent(input$change_dataset, {
    print("Button clicked")
    shinyjs::hide("main_content")
    shinyjs::show("main_content")
    updateNavbarPage(session, "main", selected = "datasetchoicevalue")
    print("Navbar page updated")
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
