server <- function(input, output, session) {
  
  flog.info("Default dataset preloaded: %s", !is.null(default_dataset))
  flog.info("Variables to display: %s", paste(variable_to_display, collapse = ", "))
  flog.info(sprintf("Columns for new dataset loaded %s", colnames(default_dataset$initial_data)))
  if(is.logical(debug) && debug){
    debug = TRUE
  } else {
    debug = FALSE
  }
  
  
  # Initialize resource paths and modules
  # addResourcePath("www", here::here("www"))
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
  wkt <- reactiveVal(global_wkt)
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
      dataset_not_init <- load_query_data(selected_dataset, selected_gridtype, selected_measurement_unit,debug = debug, pool)
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
      flog.info("Palettes initialised session")
      flog.info("Reloading session")
      # source(here::here("tab_panels/sidebar_ui_with_variable_to_display.R"))
      # source(here::here("ui.R"))
      
      initial_data(default_dataset$initial_data)
      data_for_filters(default_dataset$data_for_filters)
      flog.info(sprintf("colnames %s", colnames(default_dataset)))
      # session$reload() # ne relance pas global.R
      flog.info(sprintf("Launching global.R again"))
      
      shinyjs::refresh() #relance global.R
      
      shinyjs::hide("loading_page")
      
      showNotification("Dataframe loaded", type = "message", id = "loadingbigdata")
      shinyjs::show("main_content")
      wkt(global_wkt)
      
      shinyjs::delay(1000,{
        shinyjs::click("submit")
        data_for_filters_trigger(data_for_filters_trigger() + 1)
        updateNavbarPage(session, "main", selected = "generaloverview")
        
      })
      
    }
    
    
  })
  
  shinyjs::delay(1, { 
    flog.info("submitfirstdataset")
    shinyjs::click("dataset_choice-submitDataset") 
  })
  
  # Filtering the final data
  final_filtered_data <- eventReactive(input$submit, {
    flog.info("Submit button clicked")
    req(wkt())
    req(initial_data())
    req(data_for_filters())
    current_wkt <- wkt()
    
    if (!firstSubmit()) {
      showNotification("Filtering the data", type = "message", duration = NULL, id = "filtrage")
    }
    
    flog.info("Filtering")
    
    final_filtered_data <- initial_data()
    
    for (variable in variable_to_display) {
      select_input <- paste0("select_", variable)
      unique_values <- unique(final_filtered_data[[variable]])
      if (!is.null(input[[select_input]]) && !all(unique_values %in% input[[select_input]])) {
        final_filtered_data <- final_filtered_data %>%
          dplyr::filter(!!sym(variable) %in% input[[select_input]])
      }
    }
    
    if (!is.null(input$toggle_year) && !is.null(input$years)) {
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(year %in% (if (input$toggle_year) input$years else seq(input$years[1], input$years[2])))
    }
    
    if(as.character(current_wkt) != global_wkt){
      # Your spatial filtering code
      sf_wkt <- st_as_sfc(as.character(current_wkt), crs = 4326)
      
      # Step 1: Select unique geographic identifiers and their associated geometries
      unique_id_geom <- final_filtered_data %>%
        dplyr::select(geographic_identifier, geom_wkt) %>%
        dplyr::distinct()
      
      # Step 2: Convert to sf object
      unique_id_geom <- st_as_sf(unique_id_geom)
      
      # Step 3: Perform spatial intersection
      within_unique <- st_within(unique_id_geom, sf_wkt, sparse = FALSE)
      
      # Step 4: Filter based on spatial intersection
      unique_id_geom_filtered <- unique_id_geom[rowSums(within_unique) > 0, ]
      
      # Step 5: Filter the original data
      final_filtered_data <- final_filtered_data %>%
        dplyr::filter(geographic_identifier %in% unique_id_geom_filtered$geographic_identifier)
    }
    
    if (!firstSubmit()) {
      showNotification("Filtering finished", type = "message", id = "filtrage")
    }
    
    
    flog.info("Filtering finished")
    firstSubmit(FALSE)
    submitTrigger(FALSE)
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
  
  variable_choicesintersect <- reactive({
    req(data_for_filters())
    variable_choicesintersect <- intersect(colnames(data_for_filters()), variable_to_display)
    flog.info("variable_choicesintersect %s", variable_choicesintersect)
    
    variable_choicesintersect
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
  
  original_data <- reactiveVal()
  observe({
    original_data(initial_data())  # Store original data at start
  })
  
  # Reactive variable to store CSV data
  csv_data <- reactiveVal()
  
  # Observe CSV file upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    data <- read.csv(input$file_upload$datapath)
    csv_data(data)  # Store uploaded data
    flog.info("Csv_data uploaded")
  })
  
    # Apply filters based on uploaded CSV
  observeEvent(input$apply_csv_filters, {
    flog.info("Apply csv filters")
    req(csv_data())
    flog.info("Req csv data")
    # Implement filtering logic using csv_data()
    # Example: filtering by a column that both datasets share
    common_columns <- intersect(colnames(initial_data()), colnames(csv_data()))
    filtered_data <- initial_data()
    
    filtered_data <- filtered_data %>% dplyr::mutate_if(is.double, as.character)
    csv_data <- csv_data() %>% dplyr::mutate_if(is.double, as.character)
    
    filtered_data <- filtered_data %>% dplyr::inner_join(csv_data)
    browser()
    filtered_data <- as.data.frame(filtered_data %>% dplyr::ungroup() %>% 
                                     dplyr::mutate(year = as.numeric(year), month = as.numeric(month)) %>% 
                                     dplyr::mutate(measurement_value = as.numeric(measurement_value)))
    
    data <- load_initial_data(filtered_data)
    
    flog.info("Initial Data loaded")
    
    initial_data(data$initial_data)
    flog.info("Inital data loaded")
    
    data_for_filters(data$data_for_filters)

    flog.info("Data is filtered on the basis of the csv")
    flog.info(paste0("Initial data is filtered, nrow are:", nrow(initial_data())))
    
    # session$reload() # ne relance pas global.R
    # shinyjs::refresh() #relance global.R
    # 
    # showNotification("Dataframe loaded", type = "message", id = "loadingbigdata")
    # shinyjs::show("main_content")
    # wkt(global_wkt)
    
    shinyjs::delay(1000,{
      shinyjs::click("submit")
      data_for_filters_trigger(data_for_filters_trigger() + 1)
      updateNavbarPage(session, "main", selected = "generaloverview")
      
    })
    
    
    output$filtered_data_table <- renderDT({
      initial_data()
    })
  })
  
  # Reset filters to original data
  observeEvent(input$reset_csv_filters, {
    initial_data(original_data())  # Reset to original unfiltered data
    output$filtered_data_table <- renderDT({
      initial_data()
    })
  })
  
  # Ensure the data table shows the current state of initial_data
  output$filtered_data_table <- renderDT({
    head(initial_data())
  })
  
  # Update selectors and reactivity
  
  # ObserveEvent for the resetFilters button
  observeEvent(input$resetFilters, {
    flog.info("Resetting filters")
    data_for_filters_trigger(data_for_filters_trigger() + 1)
  })
  
  observeEvent(data_for_filters_trigger(), {
    req(data_for_filters())
    req(variable_choicesintersect())
    data_for_filters <- data_for_filters()
    lapply(variable_choicesintersect(), function(variable) {
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
  
  newwkttest <- reactiveVal(NULL)
  
  # Map and time series
  # Loop through variables and set up the modules
  lapply(variable_to_display, function(variable) {
    local({
      variable <- variable
      pieMapTimeSeriesServer(
        paste0(variable, "_module"), 
        category_var = variable, 
        data = final_filtered_data, 
        centroid = centroid, 
        submitTrigger = submitTrigger, 
        newwkttest = newwkttest  # Pass the single newwkt reactive value to each module
      )
    })
  })
  
  observeEvent(newwkttest(), {
    req(newwkttest())  # Ensure newwkt is not NULL
    flog.info("New WKT received: %s", newwkttest())
    wkt(newwkttest())
    # Trigger any further actions, such as filtering based on the new WKT
    submitTrigger(TRUE)
  })
  
  # Pie charts
  lapply(variable_to_display, function(variable) {
    local({ # to isolate each variable in its own environement, otherwise sometimes its only one of the variables that is displayed
      variable <- variable
      categoryGlobalPieChartServer(paste0(variable, "_chart"), variable, data_without_geom)
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
    removeModal()
    submitTrigger(TRUE)
  })
  
  observeEvent(submitTrigger(), {
    flog.info(sprintf("submittrigger is %s", submitTrigger()))
    req(wkt())
    if(submitTrigger()) {
      flog.info("submit")
      shinyjs::click("submit")  # Trigger the submit button click in the sidebar
      submitTrigger(FALSE)
    }
  })
  
  output$sidebar_ui_with_variable_to_display <- renderUI({
    req(variable_choicesintersect())
    variable_choicesintersect <- variable_choicesintersect()
    nav_panel(
      title = "Filter your data",
      useShinyjs(),
      
      # Submit button at the top
      div(style = "position: -webkit-sticky; position: sticky; top: 0; z-index: 999;",
          actionButton("submit", "Submit", class = "btn-primary")
      ),
      tags$br(),
      
      # Year input and toggle
      uiOutput("year_input"),
      checkboxInput("toggle_year", "Discrete selection of year", value = FALSE),
      tags$br(),
      
      # Dynamic variable filters
      do.call(tagList, lapply(variable_choicesintersect, function(variable) {
        if(variable == "species"){
          tagList(
            div(uiOutput("select_species")),
            div(class = "row", 
                div(class = "col-6", 
                    actionButton("all_species", "Select All Species")
                ),
                div(class = "col-6", 
                    actionButton("major_tunas", "Select Major Tunas")
                )
            ),
            tags$br(),
            tags$br()
          )
        } else if(variable == "species_name") {
          tagList(
            div(uiOutput("select_species_name")),
            div(class = "row", 
                div(class = "col-6", 
                    actionButton("all_species_name", "Select All Species")
                ),
                div(class = "col-6", 
                    actionButton("major_tunas_name", "Select Major Tunas")
                )
            ),
            tags$br(),
            tags$br()
          )
        } else {
          tagList(
            div(
              uiOutput(paste0("select_", variable)),
              actionButton(paste0("all_", variable), paste("Select All", gsub("_", " ", variable)))
            ),
            tags$br(),
            tags$br()
          )
        }
      })),
      
      # Reset buttons
      div(class = "row", 
          div(class = "col-6", 
              actionButton("resetWkt", "Reset WKT to global")
          ),
          div(class = "col-6", 
              actionButton("resetFilters", "Reset Filters")
          )
      ),
      tags$br(),
      
      # Dataset change button
      actionButton("change_dataset", "Choose another dataset")
    )
  })
  
  
  output$dynamic_panels <- renderUI({
    req(variable_choicesintersect())
    panel_list <- lapply(variable_choicesintersect(), function(column_name) {
      nav_panel(
        title = column_name,
        geographic_catches_by_variable_ui(column_name)
      )
    })
    do.call(navset_card_tab, panel_list)
    
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
                                      geomIdAttributeName="codesource_area",
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
  
  catches_by_variable_moduleServer("catches_by_variable_month", data_without_geom)
  newwkt <- mapCatchesServer("total_catch", data = final_filtered_data, submitTrigger = submitTrigger)
  plotTotalCatchesServer("catch_by_year", data = data_without_geom)
  
  observeEvent(newwkt$newwkt(), { #if newwkt from mapCatchesserver is updated 
    req(wkt())
    if(newwkt$newwkt() != wkt()){
      wkt(newwkt$newwkt())
      submitTrigger(TRUE)
    }
  })
  
  observeEvent(firstSubmit(), {
    if (!firstSubmit()) {
      flog.info("delay")
      shinyjs::delay(100, {   
        data_for_filters_trigger(data_for_filters_trigger() + 1)
        # show(TRUE)
        flog.info("delay finished")
        shinyjs::hide("loading_page")
        shinyjs::show("main_content")
        showModal(                   modalDialog(
          title = "Information",
          # includeHTML("doc/ribbon_GH.html"),
          includeMarkdown("doc/popup.md"),
          size = "l",
          easyClose = TRUE,
          footer=modalButton("OK", icon =icon("check"))
        ))
      })
    }
  })
  
  # observeEvent(c(firstSubmit(), additionalState()), {
  #   if (firstSubmit() && additionalState()) {
  #     flog.info("delay")
  #     shinyjs::delay(100, {   
  #       data_for_filters_trigger(data_for_filters_trigger() + 1)
  #       # show(TRUE)
  #       flog.info("delay finished")
  #       removeModal()
  #       shinyjs::hide("loading_page")
  #       shinyjs::show("main_content")
  #       
  #     })
  #   }
  # })
  
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
    try(poolClose(pool))
  })
}
