server <- function(input, output, session) {
  flog.info("Default dataset preloaded: %s", !is.null(default_dataset))
  flog.info("Variables to display: %s", paste(variable_to_display, collapse = ", "))
  flog.info(sprintf("Columns for new dataset loaded %s", colnames(default_dataset$initial_data)))
  if(!exists("debug")){
    debug = FALSE
  }
  
  
  # Initialize resource paths and modules
  addResourcePath("www", here::here("www"))
  serveRmdContents("rmd_docs", nav_bar_menu_html)
  
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
  variable_to_display_react <- reactiveVal(variable_to_display)
  
  # Show main content after loading
  observeEvent(show(), {
    if (show()) {
      shinyjs::hide("loading_page")
      shinyjs::show("main_content")
      updateNavbarPage(session, "main", selected = "generaloverview")
    }
  })
  
  dataset_choices <- dataset_choice_server("dataset_choice", filters_combinations, "global_catch_5deg_1m_firms_level1", default_gridtype, default_measurement_unit)
  
  # Initial dataset submission
  observeEvent(dataset_choices$submit(), {
    flog.info("Submit dataset clicked")
    selected_dataset <- dataset_choices$selected_dataset()
    selected_gridtype <- dataset_choices$selected_gridtype()
    selected_measurement_unit <- dataset_choices$selected_measurement_unit()
    
    firstsubmit <- firstSubmit()
      if (firstsubmit) {
      flog.info("First submit")
      flog.info("All initialization files already exist. Loading from files.")
      flog.info("loading initial data")
      data <- load_initial_data(default_dataset)
      
      flog.info("Initial Data loaded")
      
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
      showNotification("Loading big dataset, please wait. ", type = "message", duration = NULL, id = "loadingbigdata")

      flog.info("Loading dataset")

      # shinyjs::hide("main_content")
      # shinyjs::show("loading_page")
      dataset_not_init <- load_query_data(selected_dataset, selected_gridtype, selected_measurement_unit,debug = TRUE, pool)
      flog.info("Default dataset loaded")
      default_dataset <- dataset_not_init$initial_data
      flog.info(sprintf("Columns for new dataset loaded %s", colnames(default_dataset)))
      # saveRDS(default_dataset, file = "default_dataset.rds")
      variable_to_display_ancient <- variable_to_display
      variable_to_display <- intersect(variable,colnames(default_dataset))
      # saveRDS(variable_to_display, file = "variable_to_display")

      flog.info(sprintf("Variable to display %s:", variable_to_display))
      for (col in variable_to_display) {
        assign(paste0("target_", col), unique(default_dataset[[col]]), envir = .GlobalEnv)
        flog.info(sprintf("Target assigned %s:", col))

      }
      analysis_options <- lapply(variable_to_display, generate_analysis_option)
      dimensions <- lapply(variable_to_display, generate_dimension)
      targetVariables <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)

      targetVariables2 <- lapply(targetVariables, as.data.frame)

      targettes <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)
      # # Initialize color palettes with a fixed seed for reproducibility
      palettes <- initialiserPalettes(targetVariables2, seed = 2643598)
      default_dataset <<- default_dataset
      dimensions <<- dimensions
      variable_to_display <<- variable_to_display
      palettes <<- palettes
      # assign("default_dataset", default_dataset, envir =  .GlobalEnv)
      # assign("dimensions", dimensions, envir =  .GlobalEnv)
      # assign("variable_to_display", variable_to_display, envir =  .GlobalEnv)
      # assign("palettes", palettes, envir =  .GlobalEnv)
      # assign("palettes", palettes, envir =  .GlobalEnv)
      # flog.info("Palettes initialised session")
      # flog.info("Reloading session")
      # assign("default_dataset", default_dataset, envir = current_env())
      # assign("dimensions", dimensions, envir =   current_env())
      # assign("variable_to_display", variable_to_display, envir =   current_env())
      # assign("palettes", palettes, envir =   current_env())
      # assign("palettes", palettes, envir =   current_env())
      flog.info("Palettes initialised session")
      flog.info("Reloading session")
      source(here::here("tab_panels/sidebar_ui_with_variable_to_display.R"))
      source(here::here("ui.R"))
      
      initial_data(default_dataset$initial_data)
      data_for_filters(default_dataset$data_for_filters)
      # if(variable_to_display != variable_to_display_ancient){
      flog.info(sprintf("colnames %s", colnames(default_dataset)))
      # session$reload() # ne relance pas global.R
      # }
      shinyjs::refresh() #relance global.R
      # session$onSessionEnded(restart_app)
      # session$close()

      shinyjs::hide("loading_page")

      showNotification("Dataframe loaded", type = "message", id = "loadingbigdata")
      shinyjs::show("main_content")
      wkt(global_wkt)
      
      
    }
    
   
    shinyjs::delay(1000,{
    shinyjs::click("submit")
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
  
  # onStop(function() {
  #   poolClose(pool)
  # })
}
