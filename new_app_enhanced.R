source("global.R")

####################################################################################################################################################################################################################################
# pool <- connect_to_db()
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 

# target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.i6i7i8 ORDER BY dataset;")
# target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.i6i7i8 ORDER BY species;")
# target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.i6i7i8 ORDER BY year;")
# target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.i6i7i8 ORDER BY fishing_fleet;")
# target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.i6i7i8 ORDER BY gridtype;")
# filters_combinations <- dbGetQuery(pool, "SELECT dataset, gridtype, species, year, fishing_fleet FROM  public.i6i7i8 GROUP BY dataset, gridtype, species, year, fishing_fleet;")

default_gridtype <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  head(1) %>% 
  pull(gridtype)

default_species <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  dplyr::filter(gridtype == default_gridtype) %>% 
  head(1) %>% 
  pull(species)

default_flag <- unique(filters_combinations$fishing_fleet)

# default_dataset <- ifelse('global_catch_firms_level0' %in%target_dataset$dataset, "global_catch_firms_level0", target_dataset$dataset[1])
# default_gridtype <- dbGetQuery(pool, paste0("SELECT DISTINCT(gridtype) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' LIMIT 1;"))$gridtype
# default_species <- dbGetQuery(pool, paste0("SELECT DISTINCT(species) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND gridtype = '", default_gridtype, "' ORDER BY species;"))[[1]][1]
# default_year <- dbGetQuery(pool, paste0("SELECT DISTINCT(year) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND gridtype = '", default_gridtype, "' AND species = '", default_species, "' LIMIT 1;"))
# default_flag <- ifelse('EUFRA' %in%target_flag, "EUFRA", target_flag[[1]][1])

onStop(function() {
  poolClose(pool)
})
require(bslib)
require(gridlayout)
require(here)
require(RColorBrewer)

source(here::here("R/palette_species_setting.R"))

####################################################################################################################################################################################################################################
source(here::here('tab_panels/geographic_catches_ui.R'))
source(here::here('tab_panels/main_panel_ui.R'))
source(here::here('tab_panels/geographic_catches_by_species_ui.R'))
source(here::here('tab_panels/geographic_catches_by_fishing_fleet_ui.R'))
source(here::here('tab_panels/ggplot_indicator_11_ui.R'))
source(here::here('tab_panels/zoom_level_ui.R'))
source(here::here('tab_panels/additional_info_ui.R'))
source(here::here('tab_panels/filterUI.R'))
source(here::here('tab_panels/data_explorer_overview_ui.R'))
source(here::here('tab_panels/total_catch_plot.R'))
source(here::here('tab_panels/sidebar_ui.R'))
source(here::here('tab_panels/mapCatchesmodules.R'))

ui <- page_navbar(
  title = "Tuna Atlas: Interactive Indicator",
  selected = "GTA Shiny App",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  sidebar = sidebar_ui(),
  main_panel_ui(),
  geographic_catches_ui(),
  geographic_catches_by_species_ui(),
  geographic_catches_by_fishing_fleet_ui(),
  # ggplot_indicator_11_ui(),
  zoom_level_ui(),
  data_explorer_overview_ui(),  additional_info_ui()
  
)


server <- function(input, output, session) {
  pool <- connect_to_db()
  
  output$select_dataset <- renderUI({
    # datasets <- unique(filters_combinations$dataset)
    # datasets <- dbGetQuery(pool, "SELECT DISTINCT dataset FROM public.i6i7i8 ORDER BY dataset;")
    # selectizeInput('dataset', 'Select the Dataset', choices = datasets)
    datasets <- filters_combinations %>% dplyr::select(dataset) %>% distinct()
    selectizeInput('select_dataset', 'Select the Dataset', choices = datasets$dataset, selected = default_dataset)
  })
  
  output$select_gridtype <- renderUI({
    req(input$select_dataset)
    # gridtypes <- dbGetQuery(pool, sprintf("SELECT DISTINCT gridtype FROM public.i6i7i8 WHERE dataset = '%s' ORDER BY gridtype;", input$select_dataset))
    gridtypes <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>%
      dplyr::select(gridtype) %>%
      distinct()
    
    selectizeInput('select_gridtype', 'Select the Grid Type', choices = gridtypes$gridtype, selected = default_gridtype)
  })
  
  output$select_species <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    # species <- dbGetQuery(pool, sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", input$select_dataset, input$select_gridtype))
    
    species <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% 
      dplyr::filter(gridtype == input$select_gridtype) %>% 
      dplyr::select(species) %>% 
      distinct()
    selectizeInput('select_species', 'Select Species', choices = species$species, multiple = TRUE, selected = default_species)
    
  })
  
  output$select_fishing_fleet <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    # fleets <- dbGetQuery(pool, sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", input$select_dataset, input$select_gridtype))
    
    fleets <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% 
      dplyr::filter(gridtype == input$select_gridtype) %>% 
      dplyr::select(fishing_fleet) %>% 
      distinct()
    selectizeInput('select_fishing_fleet', 'Select the Fishing Fleet', choices = fleets$fishing_fleet, multiple = TRUE, selected = default_flag)
    
  })
  
  
  
  output$select_year <- renderUI({
    req(input$select_dataset, input$select_gridtype)
    years <- dbGetQuery(pool, sprintf("SELECT MIN(year) as min_year, MAX(year) as max_year FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s';", input$select_dataset, input$select_gridtype))
    sliderInput('select_year', 'Select the year range', min = years$min_year, max = years$max_year, value = c(years$min_year, years$max_year))
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
  
  totalCatchplotUIServer("plot_by_variable")
  
  
  sql_query_metadata_plot1 <- eventReactive(input$submit, {
    paste0("Your zoom is Zoom",zoom(),"   ;")
  },
  ignoreNULL = FALSE)
  
  sql_query = eventReactive(input$submit, {
    # if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, fishing_fleet, SUM(measurement_value) as measurement_value,
  ST_asText(geom) AS geom_wkt, year FROM public.i6i7i8
      WHERE dataset IN ({dataset_name})
      AND ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND fishing_fleet IN ({fishing_fleet_name*})
      AND species IN ({species_name*})
      AND year BETWEEN ({start_year*}) AND ({end_year*})
      GROUP BY species, fishing_fleet,geom_id, geom_wkt, geom , year
      ORDER BY species,fishing_fleet DESC", 
      wkt = wkt(),
      dataset_name = input$select_dataset, 
      species_name = input$select_species,
      fishing_fleet_name = input$select_fishing_fleet,
      start_year = input$select_year[1],
      end_year = input$select_year[2],
      .con = pool)
  }, ignoreNULL = FALSE)
  
  # sql_query_species_pie <- eventReactive(input$submit, {
  #   if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
  #   query <- glue::glue_sql(
  #     "SELECT   geom_id, geom, species, SUM(measurement_value) as measurement_value, ST_asText(geom) AS geom_wkt FROM public.i6i7i8
  #     WHERE dataset IN ({dataset_name})
  #     WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
  #     AND fishing_fleet IN ({fishing_fleet_name*})
  #     AND year IN ({year_name*})
  #     GROUP BY species, geom_id, geom_wkt, geom
  #     ORDER BY measurement_value DESC",
  #     wkt = wkt(),
  #     species_name = input$species,
  #     fishing_fleet_name = input$fishing_fleet,
  #     year_name = year_name,
  #     dataset_name = input$select_dataset,
  #     .con = pool)
  # },
  # ignoreNULL = FALSE)
  
  # sql_query_metadata<- NULL
  sql_query_metadata <- eventReactive(input$submit, {
    paste0("SELECT species, fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, fishing_fleet, geom")
  },
  ignoreNULL = FALSE)
  
  
  data <- eventReactive(input$submit, {
    outp <- st_read(pool, query = sql_query())
    outp
  },
  ignoreNULL = FALSE)
  
  sum_all <- reactive({
    st_read(pool, query = paste0("SELECT geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY geom")) 
  }) 
  
  mapCatchesServer("total_catch", sum_all) 
  
  
  sum_species <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom")) 
  })  
  
  sum_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, geom")) 
  })
  
  data_pie_map_fishing_fleet <- reactive({
    # st_read(pool, query = paste0("SELECT species, fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(SELECT geom_id, geom, species, fishing_fleet, SUM(measurement_value) as measurement_value, ST_asText(geom) AS geom_wkt, year FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND species IN ('YFT') AND fishing_fleet IN ('EUESP', 'EUFRA', 'JPN', 'TWN') AND year IN ('2010') GROUP BY species, fishing_fleet,geom_id, geom_wkt, geom , year ORDER BY species,fishing_fleet DESC) AS foo GROUP BY species, fishing_fleet, geom"))
    st_read(pool, query = paste0("SELECT fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, geom")) %>% 
      spread(fishing_fleet, measurement_value, fill = 0) %>%
      dplyr::mutate(total = rowSums(across(any_of(target_flag$fishing_fleet))))
  })
  
  data_pie_map_species <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom"))  %>% 
      spread(species, measurement_value, fill=0)  %>%  
      dplyr::mutate(total = rowSums(across(any_of(as.vector(target_species$species)))))  })
  
  data_time_serie <- reactive({
    st_read(pool, query = paste0("SELECT to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY year")) 
  })
  
  data_time_serie_species <- reactive({
    st_read(pool, query = paste0("SELECT species,to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, year")) 
  })
  
  data_time_serie_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet,to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet, year"))
  })
  
  
  centroid <- eventReactive(input$submit, {
    st_read(pool, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;"))
  },
  ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   centroid(st_read(pool, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;")))
  # },
  # ignoreInit = FALSE)
  
  # metadata <- eventReactive(input$submit, {
  #   st_read(pool, query = sql_query_metadata())
  # },
  # ignoreNULL = FALSE)
  
  # data <- eventReactive(input$submit, {
  #   st_read(pool, query = sql_query())
  # },
  # ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   data(st_read(pool, query = sql_query()))
  # },
  # ignoreInit = FALSE)
  
  
  # observeEvent(data(), {
  #   # metadata(st_read(pool, query = sql_query_metadata()))
  #   metadata(data()  %>% group_by(species,geom_wkt) %>% summarise(measurement_value = sum(measurement_value)))
  # },
  # ignoreInit = FALSE)
  # 
  # observeEvent(data(), {
  #   data_i11(data(data() %>% group_by(species,fishing_fleet,geom_wkt) %>% summarise(measurement_value = sum(measurement_value)) %>% spread(fishing_fleet, measurement_value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$fishing_fleet)))))))
  # },
  # ignoreInit = FALSE)
  
  
  # data_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,fishing_fleet,geom_wkt) %>% summarise(measurement_value = sum(measurement_value)) %>% spread(fishing_fleet, measurement_value, fill=0)  %>% 
  #   #   mutate(total = rowSums(across(any_of(as.vector(input$fishing_fleet)))))
  #   data()  %>% group_by(species,fishing_fleet,geom_wkt) %>% summarise(measurement_value = sum(measurement_value)) %>% spread(fishing_fleet, measurement_value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$fishing_fleet))))) %>% filter (total>mean(total))
  #   # data() %>% spread(fishing_fleet, measurement_value, fill=0)  %>% mutate(total = rowSums(across(any_of(as.vector(input$fishing_fleet)))))
  #   # st_read(pool, query = "SELECT ogc_fid, geom_id, geom, year, species, fishing_fleet, measurement_value, count,ST_asText(geom) AS geom_wkt FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND fishing_fleet IN ('EU.ESP','JPN','TWN') AND year IN ('2014')") %>% group_by(species,fishing_fleet,geom_wkt) %>% summarise(measurement_value = sum(measurement_value)) %>% spread(fishing_fleet, measurement_value)  %>%
  #   #   replace(is.na(.), 0) %>% mutate(total = rowSums(across(all_of(c("JPN","TWN"))))) %>% class()
  #     # mutate(total = rowSums(across(all_of(c("JPN","TWN")))))
  #      # rowwise()  %>% mutate(sumrow = as_data_frame(.)[,-c(1:3)])     replace(is.na(.), 0) %>%    all_of(input$fishing_fleet)))    mutate(sum = rowSums(across(where::here(is.numeric)))))
  # },
  # ignoreNULL = FALSE)
  
  
  
  
  
  # metadata_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(fishing_fleet) %>% summarise(measurement_value = sum(measurement_value))  %>% arrange(desc(measurement_value)) # %>% top_n(3)
  #   data() %>% group_by(fishing_fleet) %>% summarise(measurement_value = sum(measurement_value))  %>% arrange(desc(measurement_value)) # %>% top_n(3)
  #   
  # },
  # ignoreNULL = FALSE)
  
  
  
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
  
  output$zoom <- renderText({ 
    paste0("Your zoom is Zoom ",zoom(),"   ;")
  })
  
  
  output$DT <- renderDT({
    data()  %>% st_drop_geometry()
  }) 
  
  
  output$DTi11 <- renderDT({
    data_pie_map_species()  %>% st_drop_geometry()
    
  }) 
  
  
   
  
  
  
  
  output$total_catch_species <- renderLeaflet({
    
    df <- sum_species()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    qpal <- colorQuantile(rev(viridis::viridis(10)),df$measurement_value, n=10)
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    # https://rstudio.github.io/leaflet/showhide.html
    mymap <- leaflet() %>% 
      addProviderTiles("Esri.NatGeoWorldMap") %>% 
      clearBounds() %>%
      addPolygons(data = df,
                  label = ~measurement_value,
                  popup = ~paste0("Total catches for ",species," species in this square of the grid: ", round(measurement_value), " tons (t) et des brouettes"),
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
      leaflet::addLegend("bottomright", pal = qpal, values = df$measurement_value,
                         title = "Total catch per cell for selected criteria",
                         labFormat = labelFormat(prefix = "MT "),
                         opacity = 1
      )
  })
  

  
  
  
  
  output$plot_fishing_fleet<- renderPlotly({ 
    df_i2 = st_read(pool, query = paste0("SELECT fishing_fleet, count(fishing_fleet), sum(measurement_value) FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY fishing_fleet ORDER BY count;")) 
    
    # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
    # barplot(as.vector(as.integer(df_i2$count)),names.arg=df_i2$fishing_fleet, xlab="fishing_fleet",ylab="count",las = 2, cex.names = 1)
    la_palette_fishing_fleet = palette_fishing_fleet[names(palette_fishing_fleet) %in% unique(df_i2$fishing_fleet)]
    
    
    fig <- plot_ly(df_i2, labels = ~fishing_fleet, values = ~count, type = 'pie',
                   marker = list(colors = la_palette_fishing_fleet, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Overall fishing_fleet composition in selected area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  output$plot_species<- renderPlotly({ 
    df_i2 = st_read(pool, query = paste0("SELECT species, count(species), sum(measurement_value) FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species ORDER BY count;")) 
    
    # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
    # barplot(as.vector(as.integer(df_i2$count)),names.arg=df_i2$species, xlab="species",ylab="count",las = 2, cex.names = 1)
    la_palette_species = palette_species[names(palette_species) %in% unique(df_i2$species)]
    
    
    fig <- plot_ly(df_i2, labels = ~species, values = ~count, type = 'pie',
                   marker = list(colors = la_palette_species, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Overall species composition in selected area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  
  # https://francoisguillem.shinyapps.io/shiny-demo/ => ADD TIME TO PLAY A VIDEO !!
  
  
  
  
  
  
  
  
  # output$pie_map_i11 <- renderPlotly({
  output$plot_fishing_fleet <- renderPlotly({
      metadata_i11 <- data_pie_chart_fishing_fleet() 
    df_i11_map <- as_tibble(metadata_i11)  
    
    la_palette = palette3[names(palette3) %in% unique(df_i11_map$fishing_fleet)]    
    
    fig <- plot_ly(df_i11_map, labels = ~fishing_fleet, values = ~measurement_value, type = 'pie',
                   marker = list(colors = la_palette, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Tuna catches by fishing_fleet for selected species, area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
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
  
  
  output$plot_species_by_time <- renderDygraph({
    df_i1 <- data_time_serie_species()
    
    # Reshape the data so that each species becomes a column
    df_wide <- df_i1 %>%
      tidyr::spread(key = species, value = measurement_value, fill = 0)
    
    # Convert to xts object
    tuna_catches_timeSeries <- xts(df_wide[-1], order.by = as.Date(df_wide$year))
    
    # Plot the dygraph with one line per species
    g1 <- dygraph(tuna_catches_timeSeries) %>%
      dyOptions(fillGraph = TRUE) %>%
      dyGroup(colnames(tuna_catches_timeSeries)) %>%
      dyRangeSelector()
    
    g1
  })
  
  
  output$plot_fishing_fleet_by_time <- renderDygraph({
    df_i1 <- data_time_serie_fishing_fleet()
    
    # Reshape the data so that each fishing_fleet becomes a column
    df_wide <- df_i1 %>%
      tidyr::spread(key = fishing_fleet, value = measurement_value, fill = 0)
    
    # Convert to xts object
    tuna_catches_timeSeries <- xts(df_wide[-1], order.by = as.Date(df_wide$year))
    
    # Plot the dygraph with one line per fishing_fleet
    g1 <- dygraph(tuna_catches_timeSeries) %>%
      dyOptions(fillGraph = TRUE) %>%
      dyGroup(colnames(tuna_catches_timeSeries)) %>%
      dyRangeSelector()
    
    g1
  })
  
  
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
  
  
  output$pie_map_fishing_fleet <- renderLeaflet({
    data_pie_map_fishing_fleet <- data_pie_map_fishing_fleet()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    la_palette_fishing_fleet = palette_fishing_fleet[names(palette_fishing_fleet) %in% unique(data_pie_map_fishing_fleet$fishing_fleet)]
    la_palette_fishing_fleet = palette_fishing_fleet[names(palette_fishing_fleet) %in% colnames(dplyr::select(data_pie_map_fishing_fleet,-total))]
    
    data_pie_map_fishing_fleet <-  leaflet() %>%
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
      addMinicharts(lng = st_coordinates(st_centroid(data_pie_map_fishing_fleet, crs = 4326))[, "X"],
                    lat = st_coordinates(st_centroid(data_pie_map_fishing_fleet, crs = 4326))[, "Y"],
                    maxValues = max(data_pie_map_fishing_fleet$total),
                    chartdata = dplyr::select(data_pie_map_fishing_fleet,-total) %>% st_drop_geometry(),type = "pie",
                    colorPalette = unname(la_palette_fishing_fleet),
                    width = (60*data_pie_map_fishing_fleet$total/max(data_pie_map_fishing_fleet$total))+20,
                    legend = TRUE, legendPosition = "bottomright") %>%
      addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
  })
  
  
  output$pie_map_species <- renderLeaflet({
    data_pie_map_species <- data_pie_map_species()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    la_palette_species = palette_species[names(palette_species) %in% unique(data_pie_map_species$species)]
    la_palette_species = palette_species[names(palette_species) %in% colnames(dplyr::select(data_pie_map_species,-total))]
    
    data_pie_map_species <-  leaflet() %>%  
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
      addMinicharts(lng = st_coordinates(st_centroid(data_pie_map_species, crs = 4326))[, "X"],
                    lat = st_coordinates(st_centroid(data_pie_map_species, crs = 4326))[, "Y"],
                    maxValues = max(data_pie_map_species$total),
                    chartdata = dplyr::select(data_pie_map_species,-total) %>% st_drop_geometry(),type = "pie",
                    colorPalette = unname(la_palette_species),
                    width = (60*data_pie_map_species$total/max(data_pie_map_species$total))+20,
                    legend = TRUE, legendPosition = "bottomright") %>% 
      addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
  })
  
  
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


# observe({
#   #use the draw_stop event to detect when users finished drawing
#   feature <- input$mymap_draw_new_feature
#   req(input$mymap_draw_stop)
#   print(feature)
#   polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
#   # see  https://rstudio.github.io/leaflet/shiny.html
#   bb <- input$mymap_bounds 
#   geom_polygon <- input$mymap_draw_new_feature$geometry
#   # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
#   geoJson <- geojsonio::as.json(feature)
#   # spdf <- geojsonio::geojson_sp(feature)
#   geom <- st_read(geoJson)
#   wkt(st_as_text(st_geometry(geom[1,])))
#   coord <- st_as_text(st_geometry(geom[1,]))
#   
#   north <- polygon_coordinates[[1]][[1]]
#   south <- polygon_coordinates[[2]][[1]]
#   east <- polygon_coordinates[[1]][[2]]
#   west <- polygon_coordinates[[2]][[2]]
#   
#   
#   if(is.null(polygon_coordinates))
#     return()
#   text<-paste("North ", north, "South ", east)
#   
#   mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
#   textOutput("wkt")
#   
# })
# observeEvent(input$refresh_map,{
#   new_zoom <- input$map_i11_zoom
#   req(input$map_i11_zoom)
#   if(zoom()!=new_zoom & !is.null(input$map_i11_zoom)){
#     la_palette = palette3[names(palette3) %in% colnames(dplyr::select(data_pie_map(),-c(species,total)))]
#     zoom(new_zoom)
#     lat_centroid <-input$map_i11_center[2]
#     lon_centroid <- input$map_i11_center[1]
#     map_i11_proxy = leafletProxy("map_i11") %>% clearMinicharts() %>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>% 
#       addMinicharts(lng = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "X"],
#                     lat = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "Y"],
#                     maxValues = max(data_pie_map()$total),
#                     transitionTime = 750,
#                     chartdata = dplyr::select(data_pie_map(),-c(species,total)) %>% st_drop_geometry(),type = "pie",
#                     colorPalette = unname(la_palette),
#                     width = 10+(zoom()^2+200*(data_pie_map()$total/max(data_pie_map()$total))),
#                     legend = TRUE, legendPosition = "bottomright")
#     
#     
#   }
# })

