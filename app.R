source("global.R")

ui <- page_navbar(id = "main",
  title = "Tuna Atlas: Interactive Indicator",
  selected = "mainpanel",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar_ui(),
  main_panel_ui(), dataset_choice(),
  geographic_catches_ui(),
  geographic_catches_by_variable_ui("species"),
  geographic_catches_by_variable_ui("fishing_fleet"),
  # ggplot_indicator_11_ui(),
  data_explorer_overview_ui(), data_explorer_i11_ui(), sqlqueriesui(),
  # additional_info_ui(),
  more_about()
  
  )

pool <- connect_to_db()

server <- function(input, output, session) {
  
  addResourcePath("rmd", here::here("rmd"))
  serveRmdContents("rmd_docs", list_markdown_path)# to create rmd tabpanels
  
  
    shinyjs::onclick("fishing_fleet_toggle", {
    shinyjs::toggle("fishing_fleet_panel");
    shinyjs::runjs('$("#arrow_indicator").html() == "&#9660;" ? $("#arrow_indicator").html("&#9650;") : $("#arrow_indicator").html("&#9660;");');
  })
  
  shinyjs::delay(500, {
    nav_select(id = "main", selected = "datasetchoicevalue")
  })
  
  shinyjs::delay(1500, {
    nav_select(id = "main", selected = "mainpanel")
  })
  
  output$select_dataset <- renderUI({
    datasets <- filters_combinations %>% dplyr::select(dataset) %>% distinct()
    selectizeInput('select_dataset', 'Select the Dataset', choices = datasets$dataset, selected = default_dataset)
  })
  
  output$select_gridtype <- renderUI({
    req(input$select_dataset)
    gridtypes <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>%
      dplyr::select(gridtype) %>%
      distinct()
    selectizeInput('select_gridtype', 'Select the Grid Type', choices = gridtypes$gridtype, selected = default_gridtype)
  })
  
  output$select_species <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    species <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% 
      dplyr::filter(gridtype == input$select_gridtype) %>% 
      dplyr::select(species) %>% 
      distinct()
    selectizeInput('select_species', 'Select Species', choices = species$species, multiple = TRUE, selected = default_species)
    
  })
  
  output$select_fishing_fleet <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    fleets <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% 
      dplyr::filter(gridtype == input$select_gridtype) %>% 
      dplyr::select(fishing_fleet) %>% 
      distinct()
    selectizeInput('select_fishing_fleet', 'Select the Fishing Fleet', choices = fleets$fishing_fleet, multiple = TRUE, selected = default_flag)
    
  })
  
  output$year_input <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    years <- dbGetQuery(pool, sprintf("SELECT MIN(year) as min_year, MAX(year) as max_year FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s';", input$select_dataset, input$select_gridtype))
    if(input$toggle_year) {
      # Mode sélection multiple
      selectInput("years", "Choose discrete(s) year", 
                  choices =c(years$min_year: years$max_year), selected = round(mean(years$min_year, years$max_year)), multiple = TRUE)
    } else {
      # Mode sélection plage d'années
      sliderInput("years", "Choose a period", 
                  min = years$min_year, max = years$max_year, value = c(years$min_year, years$max_year))
    }
  })
  
  # Implement "Select All" functionality for species
  observeEvent(input$all_species, {
    species <- dbGetQuery(pool, sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", input$select_dataset, input$select_gridtype))
    updateSelectInput(session, "select_species", selected = species$species)
  })
  
  # Implement "Select major species" functionality for species
  observeEvent(input$major_tunas, {
    species <- dbGetQuery(pool, sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", input$select_dataset, input$select_gridtype)) %>% 
      dplyr::filter(species %in% c("YFT", "SKJ", "ALB", "BET", "SBF"))
    updateSelectInput(session, "select_species", selected = species$species)
  })
  
  # Implement "Select All" functionality for fishing fleets
  observeEvent(input$all_fishing_fleet, {
    fleets <- dbGetQuery(pool, sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", input$select_dataset, input$select_gridtype))
    updateSelectInput(session, "select_fishing_fleet", selected = fleets$fishing_fleet)
  })
  
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "select_species", selected = default_species)
    updateSelectInput(session, "select_dataset", selected = default_dataset)
    updateSelectInput(session, "select_gridtype", selected = default_gridtype)
    updateSelectInput(session, "select_fishing_fleet", selected = default_flag)
  })
  
  catches_by_variable_moduleServer("catches_by_variable_month", data_without_geom)
  
  #    /* AND year BETWEEN ({start_year*}) AND ({end_year*}) */ removed as it is not only a slider input
  sql_query = eventReactive(input$submit, {
    year_vector <- if(input$toggle_year) {
        input$years
      } else {
        seq(input$years[1], input$years[2])
      }
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, fishing_fleet, SUM(measurement_value) as measurement_value,
  ST_asText(geom) AS geom_wkt, year FROM public.i6i7i8
      WHERE dataset IN ({dataset_name})
      AND ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND fishing_fleet IN ({fishing_fleet_name*})
      AND species IN ({species_name*})
      AND year IN ({selected_years*})
      GROUP BY species, fishing_fleet,geom_id, geom_wkt, geom , year
      ORDER BY species,fishing_fleet DESC", 
      wkt = wkt(),
      dataset_name = input$select_dataset, 
      species_name = input$select_species,
      fishing_fleet_name = input$select_fishing_fleet,
      selected_years = year_vector,
      .con = pool)
  }, ignoreNULL = FALSE)
  
  
  data <- eventReactive(input$submit, {
    outp <- st_read(pool, query = sql_query())
    outp
  },
  ignoreNULL = FALSE)
  
  
  data_without_geom <- reactive({
    data_without_geom <- as.data.frame(data())
    data_without_geom$geom <- NULL
    data_without_geom
    })
 
  sum_all <- reactive({
    st_read(pool, query = paste0("SELECT geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY geom")) 
  }) 
  
  sum_species <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom")) 
  })  
  
  sum_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, geom")) 
  })
  
  data_pie_map_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, geom")) %>% 
      spread(fishing_fleet, measurement_value, fill = 0) %>%
      dplyr::mutate(total = rowSums(across(any_of(target_flag$fishing_fleet))))
  })
  
  data_pie_map_species <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom")) %>% 
      spread(fishing_fleet, measurement_value, fill = 0) %>%
      dplyr::mutate(total = rowSums(across(any_of(target_flag$fishing_fleet))))
  })
  
  data_time_serie_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet,to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, year"))
  })
  
  data_time_serie <- reactive({
    st_read(pool, query = paste0("SELECT to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY year")) 
  })
  
  centroid <- reactive({
    st_read(pool, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;"))
  })
  
  sql_query_metadata <- eventReactive(input$submit, {
    paste0("SELECT species, fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, fishing_fleet, geom")
  },
  ignoreNULL = FALSE)
  
  observeEvent(input$resetWkt, {
    wkt(global_wkt)
  },
  ignoreInit = TRUE)
  
  ############################################################# OUTPUTS   ############################################################# 
  
  output$sql_query <- renderText({ 
    paste("Your SQL Query for indicator 11 is : \n", sql_query())
  })
  
  output$sql_query_metadata <- renderText({ 
    paste("Your SQL Query is : \n", sql_query_metadata())
  })
  
  output$DT <- renderDT({
    data()  %>% st_drop_geometry()
  }) 
  
  output$DTi11 <- renderDT({
    data_pie_map_species()  %>% st_drop_geometry()
  }) 
  
  
  output$plot_by_time <- renderDygraph({
    df_i1 <- data_time_serie()
    
    
    # Convert to xts object
    tuna_catches_timeSeries <- xts(df_i1[-1], order.by = as.Date(df_i1$year))
    
    # Plot the dygraph with one line per species
    g1 <- dygraph(tuna_catches_timeSeries) %>%
      dyOptions(fillGraph = TRUE) %>%
      dyGroup(colnames(tuna_catches_timeSeries)) %>%
      dyRangeSelector()
    
    g1
  })
  
  mapCatchesServer("total_catch", sum_all) 
  
  
  output$plot11 <- renderImage({
    # https://semba-blog.netlify.app/06/13/2020/plots-in-interactive-maps-with-r/
    df_i11_filtered <- as(data(), "Spatial")
    
    i11 <- Atlas_i11_CatchesByCountry(df=df_i11_filtered,
                                      geomIdAttributeName="geom_id",
                                      countryAttributeName="fishing_fleet",
                                      speciesAttributeName="species",
                                      valueAttributeName="measurement_value",
                                      withSparql=FALSE)
    
    i11
    png(i11, width = 400, height = 300)
    dev.off()
    
    # Return a list containing the filename
    list(src = i11,
         contentType = 'image/png',
         width = 1600,
         height = 1200,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  categoryGlobalPieChartServer("fishing_fleet_chart", "fishing_fleet", sql_query)
  categoryGlobalPieChartServer("species_chart", "species", sql_query)
  
  # https://francoisguillem.shinyapps.io/shiny-demo/ => ADD TIME TO PLAY A VIDEO !!
  
  pieMapTimeSeriesServer("species_module", category_var = "species", sql_query = sql_query, centroid = centroid)
  pieMapTimeSeriesServer("fishing_fleet_module", category_var = "fishing_fleet", sql_query = sql_query, centroid = centroid)
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("Tuna_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      csv_tuna = data()
      write.csv(csv_tuna, file)
    }
  )
  
  
  onStop(function() {
    poolClose(pool)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)





