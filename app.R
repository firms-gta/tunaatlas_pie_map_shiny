source("global.R")
pool <- connect_to_db()
####################################################################################################################################################################################################################################

global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 

target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.i6i7i8 ORDER BY dataset;")
target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.i6i7i8 ORDER BY fishing_fleet;")
target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.i6i7i8 ORDER BY gridtype;")

default_dataset <- ifelse('global_catch_5deg_1m_firms_level1' %in%target_dataset, "global_catch_5deg_1m_firms_level1", target_dataset[[1]][1])
default_flag <- ifelse('UNK' %in%target_flag, "UNK", target_flag[[1]][1])
default_species <- dbGetQuery(pool, paste0("SELECT DISTINCT(species) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' ORDER BY species;"))[[1]][1]
default_gridtype <- dbGetQuery(pool, paste0("SELECT DISTINCT(gridtype) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND species = '", default_species, "' LIMIT 1;"))
default_year <- dbGetQuery(pool, paste0("SELECT DISTINCT(year) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND gridtype = '", default_gridtype, "' AND species = '", default_species, "' LIMIT 1;"))

filters_combinations <- dbGetQuery(pool, "SELECT dataset, gridtype, species, year, fishing_fleet FROM  public.i6i7i8 GROUP BY dataset, gridtype, species, year, fishing_fleet;")
onStop(function() {
  poolClose(pool)
})

source("~/Documents/tunaatlas_pie_map_shiny/R/palette_species_setting.R")

####################################################################################################################################################################################################################################
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/geographic_catches.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/geographic_catches_by_species.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/geographic_catches_by_fishing_fleet.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/ggplot_indicator_11_ui.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/zoom_level_ui.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/additional_info_ui.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/filterUI.R')
source('~/Documents/tunaatlas_pie_map_shiny/tab_panels/data_explorer_overview_ui.R')


ui <- fluidPage(
     titlePanel("Tuna Atlas: indicateurs cartographiques i11"),
     navbarPage(title="TunaAtlas",
                geographic_catches_ui(),
                geographic_catches_by_species_ui(),
                geographic_catches_by_fishing_fleet_ui(),
                               # ggplot_indicator_11_ui(),
                               zoom_level_ui(),
                               data_explorer_overview_ui(),
                               additional_info_ui()
                    )
   )





server <- function(input, output, session) {
  # Render UI for dataset selection
  output$datasetUI <- renderUI({
    datasets <- dbGetQuery(pool, "SELECT DISTINCT dataset FROM i6i7i8 ORDER BY dataset;")
    selectInput("dataset", "Dataset", choices = datasets$dataset, select = default_dataset)
  })
  
  # Render UI for gridtype selection based on selected dataset
  output$gridtypeUI <- renderUI({
    req(input$dataset)
    gridtypes <- dbGetQuery(pool, sprintf("SELECT DISTINCT gridtype FROM i6i7i8 WHERE dataset = '%s' ORDER BY gridtype;", input$dataset))
    selectInput("gridtype", "Grid Type", choices = gridtypes$gridtype, selected = default_gridtype)
  })
  
  # Render UI for species selection based on selected dataset and gridtype
  output$speciesUI <- renderUI({
    req(input$dataset, input$gridtype)
    species <- dbGetQuery(pool, sprintf("SELECT DISTINCT species FROM i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", input$dataset, input$gridtype))
    selectInput("species", "Species", choices = species$species, multiple = TRUE, selected = "All")
  })
  
  # Render UI for fishing fleet selection based on selected dataset and gridtype
  output$fishingFleetUI <- renderUI({
    req(input$dataset, input$gridtype)
    fleets <- dbGetQuery(pool, sprintf("SELECT DISTINCT fishing_fleet FROM i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", input$dataset, input$gridtype))
    selectInput("fleet", "Fishing Fleet", choices = fleets$fishing_fleet, multiple = TRUE, selected = "All")
  })
  
  # Implement "Select All" functionality for species
  observeEvent(input$all_species, {
    species <- dbGetQuery(pool, sprintf("SELECT DISTINCT species FROM i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", input$dataset, input$gridtype))
    updateSelectInput(session, "species", selected = species$species)
  })
  
  # Implement "Select All" functionality for fishing fleets
  observeEvent(input$all_fishing_fleet, {
    fleets <- dbGetQuery(pool, sprintf("SELECT DISTINCT fishing_fleet FROM i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", input$dataset, input$gridtype))
    updateSelectInput(session, "fishing_fleet", selected = fleets$fishing_fleet)
  })
  
  
  observeEvent(input$resetYear, {
    updateSliderInput(session, "yearRange", value = c(1950, 2025))
    
    yearRange <- dbGetQuery(pool, sprintf("SELECT MIN(year) AS min_year, MAX(year) AS max_year FROM i6i7i8 WHERE dataset = '%s' AND gridtype = '%s';", input$dataset, input$gridtype))
    if(nrow(yearRange) > 0) {
      updateSliderInput(session, "yearRange", value = c(yearRange$min_year[1], yearRange$max_year[1]))
    } else {
      updateSliderInput(session, "yearRange", value = c(1950, 2025)) # Fallback to default range
    }
  })
  
  
  
  
sql_query_metadata_plot1 <- eventReactive(input$submit, {
    paste0("Your zoom is Zoom ",zoom(),"   ;")
  },
  ignoreNULL = FALSE)
  
  
  sql_query = eventReactive(input$submit, {
    
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, fishing_fleet, SUM(measurement_value) as measurement_value,
  ST_asText(geom) AS geom_wkt, year FROM public.i6i7i8
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND fishing_fleet IN ({fishing_fleet_name*})
      AND species IN ({species_name*})
      AND year BETWEEN ({start_year*}) AND ({end_year*})
      GROUP BY species, fishing_fleet, geom_id, geom_wkt, geom , year
      ORDER BY species,fishing_fleet DESC", 
      wkt = wkt(),
      species_name = input$species,
      fishing_fleet_name = input$fishing_fleet,
      start_year = input$year[1],
      end_year = input$year[2],
      .con = pool)
  }, ignoreNULL = FALSE)
  
  
  sql_query_metadata<- NULL
  # sql_query_metadata <- eventReactive(input$submit, {
  #   paste0("SELECT species, fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, fishing_fleet, geom")
  # },
  # ignoreNULL = FALSE)

  
  data <- eventReactive(input$submit, {
    req(input$species)
    req(input$fishing_fleet)
    req(input$year)
    outp <- st_read(pool, query = sql_query())
    outp
  },
  ignoreNULL = FALSE)
  
  
  metadata <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom")) 
  })  
  
  data_pie_map <- reactive({
    st_read(pool, query = paste0("SELECT species, fishing_fleet, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, fishing_fleet, geom")) %>% 
      spread(fishing_fleet, measurement_value, fill = 0) %>%
      dplyr::mutate(total = rowSums(across(any_of(input$fishing_fleet))))
  })
  
  data_pie_map_species <- reactive({
    st_read(pool, query = paste0("SELECT species, geom, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, geom"))  %>% 
      spread(species, measurement_value, fill=0)  %>%  
      dplyr::mutate(total = rowSums(across(any_of(as.vector(target_species$species)))))  
  })
  
  data_time_serie <- reactive({
    st_read(pool, query = paste0("SELECT species,to_date(year::varchar(4),'YYYY') AS  year, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY species, year")) 
  })
  
  data_pie_chart_fishing_fleet <- reactive({
    st_read(pool, query = paste0("SELECT fishing_fleet, sum(measurement_value) AS measurement_value FROM(",sql_query(),") AS foo GROUP BY fishing_fleet ORDER BY fishing_fleet"))
  })
  
  
  centroid <- eventReactive(input$submit, {
    st_read(pool, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;"))
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
  
  output$zoom <- renderText({ 
    paste0("Your zom is Zoom",zoom(),"   ;")
  })
  
  
  output$DT <- renderDT({
    data()  %>% st_drop_geometry()
  }) 
  
  
  output$DTi11 <- renderDT({
    data_pie_map_species()  %>% st_drop_geometry()
    
  }) 
  
  
  
  output$mymap <- renderLeaflet({
    
    
    df <- metadata()
    
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    qpal <- colorQuantile(rev(viridis::viridis(10)),df$measurement_value, n=10)
    # https://r-spatial.github.io/sf/articles/sf5.html
    # https://rstudio.github.io/leaflet/showhide.html
    mymap <- leaflet() %>% 
      addProviderTiles("Esri.NatGeoWorldMap") %>% 
      setView(lng = 0, lat = 0, zoom = 1) %>%
      clearBounds() %>%
      addPolygons(data = df,
                  label = ~measurement_value,
                  popup = ~paste0("Total catches for ",species," species in this square of the grid: ", round(measurement_value), " tons (t)"),
                  # popup = ~paste0("Captures de",species,": ", area, " tonnes(t) et des brouettes"),
                  # fillColor = ~pal_fun(measurement_value),
                  # fillColor = brewer.pal(n = 20, name = "RdBu"),
                  fillColor = ~qpal(measurement_value),
                  # color = ~pal(measurement_value)
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
  
  observe({
    #use the draw_stop event to detect when users finished drawing
    feature <- input$mymap_draw_new_feature
    req(input$mymap_draw_stop)
    print(feature)
    polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # see  https://rstudio.github.io/leaflet/shiny.html
    bb <- input$mymap_bounds
    geom_polygon <- input$mymap_draw_new_feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    geoJson <- geojsonio::as.json(feature)
    # spdf <- geojsonio::geojson_sp(feature)
    geom <- st_read(geoJson)
    wkt(st_as_text(st_geometry(geom[1,])))
    coord <- st_as_text(st_geometry(geom[1,]))

    north <- polygon_coordinates[[1]][[1]]
    south <- polygon_coordinates[[2]][[1]]
    east <- polygon_coordinates[[1]][[2]]
    west <- polygon_coordinates[[2]][[2]]


    if(is.null(polygon_coordinates))
      return()
    text<-paste("North ", north, "South ", east)

    mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
    textOutput("wkt")

  })
  
  
  
  output$plot_species<- renderPlotly({ 
    # output$plot_species<- renderPlotly({ 
    df_i2 = st_read(pool, query = paste0("SELECT species, count(species), sum(measurement_value) FROM public.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species ORDER BY count;")) # %>% filter (count>mean(count))
    
    # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
    # barplot(as.vector(as.integer(df_i2$count)),names.arg=df_i2$species, xlab="species",ylab="count",las = 2, cex.names = 1)
    la_palette_species = palette_species[names(palette_species) %in% unique(df_i2$species)]
    
    
    fig <- plot_ly(df_i2, labels = ~species, values = ~count, type = 'pie',
                   marker = list(colors = la_palette_species, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% plotly::layout(title = 'Overall species composition in selected area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  
  # https://francoisguillem.shinyapps.io/shiny-demo/ => ADD TIME TO PLAY A VIDEO !!
  output$map_i11 <- renderLeaflet({
    toto <- data_pie_map()
    
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    la_palette = palette3[names(palette3) %in% colnames(dplyr::select(toto,-c(species,total)))]
    
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_i11 <-  leaflet() %>%  
      # map_i11 <-  leaflet(options = leafletOptions(zoomSnap=0.25)) %>%  
      # setView(lng = lon_centroid, lat = lat_centroid, zoom = 3) %>% 
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
      addMinicharts(lng = st_coordinates(st_centroid(toto, crs = 4326))[, "X"],
                    lat = st_coordinates(st_centroid(toto, crs = 4326))[, "Y"],
                    # chartdata = as_data_frame(subset(toto, select = -c(species,geom_wkt))), type = "pie",
                    # chartdata = as_data_frame(toto)[-c(1:3,ncol(as_data_frame(toto)))], type = "pie",
                    maxValues = max(toto$total),
                    chartdata = dplyr::select(toto,-c(species,total)) %>% st_drop_geometry(),type = "pie",
                    # showLabels = TRUE,
                    # layerId = "tartothon",
                    # colorPalette = pal.bands(polychrome, n=36),
                    # colorPalette = d3.schemeCategory10,
                    colorPalette = unname(la_palette),
                    width = (60*toto$total/max(toto$total))+20,
                    legend = TRUE, legendPosition = "bottomright") %>% 
      addPolygons(data = toto,
                  label = ~total,
                  popup = ~paste0("Captures de",species,": ", round(total), " tonnes(t) et des brouettes"),
                  group = "grid",
                  # fillColor = ~qpal(total),
                  # fill = TRUE,
                  # fillOpacity = 0.8,
                  smoothFactor = 0.5) %>% 
      addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
  })
  
  observeEvent(input$refresh_map,{
    new_zoom <- input$map_i11_zoom
    req(input$map_i11_zoom)
    if(zoom()!=new_zoom & !is.null(input$map_i11_zoom)){
      la_palette = palette3[names(palette3) %in% colnames(dplyr::select(data_pie_map(),-c(species,total)))]
      zoom(new_zoom)
      lat_centroid <-input$map_i11_center[2]
      lon_centroid <- input$map_i11_center[1]
      #%>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>%  addProviderTiles("Esri.NatGeoWorldMap", group = "background") %>%  clearBounds() %>%
      map_i11_proxy = leafletProxy("map_i11") %>% clearMinicharts() %>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>% 
        addMinicharts(lng = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "X"],
                      lat = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "Y"],
                      maxValues = max(data_pie_map()$total),
                      transitionTime = 750,
                      chartdata = dplyr::select(data_pie_map(),-c(species,total)) %>% st_drop_geometry(),type = "pie",
                      colorPalette = unname(la_palette),
                      width = 10+(zoom()^2+200*(data_pie_map()$total/max(data_pie_map()$total))),
                      legend = TRUE, legendPosition = "bottomright")
      
      
    }
  })
  
  
  
  output$pie_map_i11 <- renderPlotly({
    metadata_i11 <- data_pie_chart_fishing_fleet() 
    df_i11_map <- as_tibble(metadata_i11) 
    
    la_palette = palette3[names(palette3) %in% unique(df_i11_map$fishing_fleet)]
    
    fig <- plot_ly(df_i11_map, labels = ~fishing_fleet, values = ~measurement_value, type = 'pie',
                   marker = list(colors = la_palette, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% plotly::layout(title = 'Catches by fishing_fleet for selected species, area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
  })
  
  
  output$plot1_streamgraph <- renderDygraph({
    df_i1 = data_time_serie() 
    
    # https://rstudio.github.io/dygraphs/gallery-timezones.html
    # create time series object
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    tuna_catches_timeSeries <- xts(x = df_i1$measurement_value, order.by = df_i1$year)
    
    # create the area chart
    g1 <- dygraph(tuna_catches_timeSeries) %>% dyOptions( fillGraph=TRUE )
    
    # create a basic interactive element
    g1 <- dygraph(g1)  %>% dyRangeSelector()
    
    g1
    
    
    
    # 
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
  
  
  
  
  output$pie_map_species <- renderLeaflet({
    toto <- data_pie_map_species()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    la_palette_species = palette_species[names(palette_species) %in% unique(toto$species)]
    la_palette_species = palette_species[names(palette_species) %in% colnames(dplyr::select(toto,-total))]
    
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
      addMinicharts(lng = st_coordinates(st_centroid(toto, crs = 4326))[, "X"],
                    lat = st_coordinates(st_centroid(toto, crs = 4326))[, "Y"],
                    maxValues = max(toto$total),
                    # chartdata = dplyr::select(toto,-c(total)) %>% st_drop_geometry(),type = "pie",
                    chartdata = dplyr::select(toto,-total) %>% st_drop_geometry(),type = "pie",
                    colorPalette = unname(la_palette_species),
                    width = (60*toto$total/max(toto$total))+20,
                    legend = TRUE, legendPosition = "bottomright") %>% 
      # addPolygons(data = toto,
      #             label = ~total,
      #             popup = ~paste0("Captures de",species,": ", round(total), " tonnes(t) et des brouettes"),
      #             group = "grid",
      #             smoothFactor = 0.5) %>% 
      addLayersControl(baseGroups = c("minicharts","grid"), overlayGroups = c("background"))
  })
  
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("Catch_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      csv_tuna = data()
      write.csv(csv_tuna, file)
    }
  )
  
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "species", selected = NULL)
    updateSelectInput(session, "gridtype", selected = NULL)
    updateSliderInput(session, "year", value = c(yearRange$min_year, yearRange$max_year))
    updateSelectInput(session, "fishing_fleet", selected = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



