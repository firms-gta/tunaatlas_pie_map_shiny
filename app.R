# pending issue with st_union
# https://keen-swartz-3146c4.netlify.app/sf.html
####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
DRV=RPostgres::Postgres()
con <- dbConnect(drv=DRV, dbname="geoflow", user="tunaatlas_inv", password="fle087", host="localhost")
####################################################################################################################################################################################################################################

global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt) 
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 
# data <- reactiveVal() 
# data_i11 <- reactiveVal() 
# data_pie_map<- reactiveVal() 
# centroid <- reactiveVal() 



target_species <- dbGetQuery(con, "SELECT DISTINCT(species) FROM fact_tables.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(con, "SELECT DISTINCT(year) FROM fact_tables.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(con, "SELECT DISTINCT(country) FROM fact_tables.i6i7i8 ORDER BY country;")

default_species <- 'YFT'
default_year <- '2010'
default_flag <- c('EUESP','EUFRA','TWN','JPN')

# default_flag <- unique(target_flag)
# default_year <- c(seq(min(target_year):max(target_year))+min(target_year)-1)

# sql_query <- reactiveVal(paste0("SELECT   geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt FROM fact_tables.i6i7i8
#            WHERE  species IN ('",paste0(default_species,collapse="','"),"')
#                       AND country IN ('",paste0(default_flag,collapse="','"),"')
#                       AND year IN ('",paste0(default_year,collapse="','"),"')
#            GROUP BY species, country,geom_wkt, geom
#            ORDER BY species,country DESC
#            ;"))


filters_combinations <- dbGetQuery(con, "SELECT species, year, country FROM  fact_tables.i6i7i8 GROUP BY species, year, country;")


# https://www.rapidtables.com/convert/color/hex-to-rgb.html
# https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  
palette3_all <- unlist(mapply(brewer.pal, 
                              palette3_info$maxcolors,
                              rownames(palette3_info)))
set.seed(2643598)  
# palette3 <- sample(palette3_all, nrow(unique(df_i11_map$country)), replace=TRUE)
palette3 <- sample(palette3_all, nrow(target_flag), replace=TRUE)
names(palette3) = target_flag$country
palette3

palette3_speciesinfo <- brewer.pal.info[brewer.pal.info$category == "qual", ]  
palette3_species <- unlist(mapply(brewer.pal, 
                                  palette3_speciesinfo$maxcolors,
                                  rownames(palette3_speciesinfo)))
set.seed(2643598)  
# palette3 <- sample(palette3_all, nrow(unique(df_i11_map$country)), replace=TRUE)
palette_species <- sample(palette3_species, nrow(target_species), replace=TRUE)
names(palette_species) = target_species$species


# print(palette3)
# class(palette3)
# palette3[names(palette3) != c('AGO','ALB')]


# palette3[-(1:10)]

# palette3_named = setNames(object = scales::hue_pal()(palette3), nm = target_flag)
# print(palette3_named)

# paldark <- colorFactor(
#   palette = 'Dark2',
#   domain = target_flag$country
# )
# print(paldark)

# # If you want to set your own colors manually:
# pal <- colorFactor(
#   palette = c('red', 'blue', 'green', 'purple', 'orange'),
#   domain = df$type
# )

####################################################################################################################################################################################################################################

ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques i11"),
  navbarPage(title="TunaAtlas", 
             tabPanel("Interactive",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 200, left = "auto", right="83%", width = "15%", height = "auto",
                                        tags$br(),
                                        
                                        h2("Select filters to customize indicators"),
                                        # imageOutput("plot11", height = 200),
                                        selectInput(
                                          inputId = "species",
                                          label = "Species",
                                          choices = target_species$species,
                                          selected= default_species
                                        ),
                                        selectInput(
                                          inputId = "year",
                                          label = "Year",
                                          choices = target_year$year,
                                          multiple = TRUE,
                                          selected= default_year
                                        ),
                                        selectInput(
                                          inputId = "country",
                                          label = "Country",
                                          choices = target_flag$country,
                                          multiple = TRUE,
                                          selected= default_flag
                                        ),
                                        actionButton(
                                          inputId = "submit",
                                          label = "Submit"
                                        ),
                                        actionButton("resetWkt", "Reset WKT to global"),
                                        # plotOutput(outputId = "plot_species",width="300")
                                        tags$br(),
                                        tags$br(),
                                        plotlyOutput(outputId = "plot_species",width="100%")
                                        
                                        # actionButton("resetWkt", "Reset WKT to global"),
                                        # plotOutput(outputId = "plot_species")
                                        # plotOutput("cumulative_plot", height="130px", width="100%")
                                        
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='89',width='108')))
                      )
             ),
             tabPanel("Interactive Indicator 11",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
                          # leafletOutput('map_i11', width = "60%", height = 1500),
                          leafletOutput("map_i11", width="100%", height="100%"),
                          
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 200, left = "auto", width = "20%", fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        # h3(textOutput("sql_query"), align = "right"),
                                        # plotOutput("plot1_streamgraph", height=200, width="100%"),
                                        # dygraphOutput("plot1_streamgraph", height=400, width="100%"),
                                        tags$br(),
                                        plotlyOutput("pie_map_i11", width="100%"),
                                        tags$br(),
                                        # h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                        # h6(textOutput("sars_reactive_country_count"), align = "right"),
                                        # plotOutput("sars_epi_curve", height="130px", width="100%"),
                                        # plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                        sliderInput(inputId="yearInterval", "Select period of interest :",
                                                    min = min(target_year),
                                                    max = max(target_year),
                                                    value = c(min(target_year),max(target_year)),
                                                    round = TRUE, step=1
                                        ),
                                        span(("Rate of catch according to the flag of the fishing fleet"),align = "left", style = "font-size:80%"),
                                        tags$br(),
                                        span(("Circles in the grid shows the detail of this rate for a spefic square of the grid"),align = "left", style = "font-size:80%"),
                                        tags$br(),
                                        tags$br(),
                                        actionButton("refresh_map","Refresh map for this zoom level")
                                        
                                        # sliderTextInput("sars_plot_date",
                                        #                 label = h5("Select mapping date"),
                                        #                 choices = format(unique(sars_cases$date), "%d %b %y"),
                                        #                 # selected = format(sars_max_date, "%d %b %y"),
                                        #                 grid = FALSE,
                                        #                 animate=animationOptions(interval = 3000, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", bottom =  "2%", left = "10%", width = "80%", fixed=TRUE, draggable = FALSE, height = "auto",
                                        dygraphOutput("plot1_streamgraph", height="400", width="80%")
                          )
                      )
             ),
             tabPanel("Interactive Indicator 11 for species",
                      div(class="outer",
                          tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
                          leafletOutput("pie_map_species", width="100%", height="100%"),

                          absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216')))
                          )
             ),
             tabPanel("ggplot Indicator 11",
                      imageOutput("plot11", height = 1200)
             ),
             tabPanel("Zoom level",
                      hr(),
                      textOutput("zoom")
             ),
             tabPanel("Data explorer overview",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DT")
                      # downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
             ),
             tabPanel("Data explorer i11",
                      # hr(),
                      # textOutput("sql_query"),
                      hr(),
                      DT::dataTableOutput("DTi11"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br()
             ),
             navbarMenu("More",
                        tabPanel(
                          title = "Your SQL query for overview",
                          textOutput("sql_query_metadata")
                        ),
                        tabPanel(
                          title = "Your SQL query",
                          textOutput("sql_query")
                        ),
                        tabPanel(
                          title = "Your SQL query plot1",
                          textOutput("sql_query_metadata_plot1")
                        ),
                        tabPanel("About",
                                 fluidRow(
                                   column(6,
                                          includeMarkdown("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/README.md")
                                   ),
                                   column(3,
                                          img(class="logo_IRD",
                                              src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                                          tags$small(
                                            "Source: IRD",
                                            "Julien Barde ",
                                            "Funding : BlueCloud ",
                                            a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                              "IRD Tuna Atlas (Alain Fontenau)"),
                                            a(href="https://github.com/juldebar/IRDTunaAtlas/wiki/Indicator-I11-:-Catches-by-country",
                                              "IRD Indicator 11"),
                                            a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                              "IRD Tuna Atlas (Alain Fontenau)"),
                                            a(href="https://horizon.documentation.ird.fr/exl-doc/pleins_textes/divers11-03/010012425.pdf",
                                              "PDF")
                                          )
                                   ),
                                   column(3,
                                          img(class="logo_IRD",
                                              src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                                          tags$small(
                                            "General Disclaimer:",
                                            "This repository contains work in progress. It can be used to explore the content of multi-dimensionnal data cubes storing tuna fisheries data. Dimensions are: spatial (lat,lon), time, flag of the fishing fleet, free schools or FADs. The content is made of publicly available data delivered by Tuna RFMOs. Its content should not be used for publications without explicit agreement from the authors. The accuracy of the estimates provided in these analyses is contingent on data quality and availability. Results presented here do not represent the official view of IRD, its staff or consultants.",
                                            "Caution must be taken when interpreting all data presented, and differences between information products published by IRD and other sources using different inclusion criteria and different data cut-off times are to be expected. While steps are taken to ensure accuracy and reliability, all data are subject to continuous verification and change.  See here for further background and other important considerations surrounding the source data."
                                          )
                                   )
                                   
                                 )
                                 # ),
                                 # 
                                 # tabPanel("About this site",
                                 #          tags$div(
                                 #            tags$h4("Last update"), 
                                 #            h6(paste0(update)),
                                 #            "This site is updated once yearly. Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
                                 #            
                                 #            tags$br(),tags$br(),tags$h4("Background"), 
                                 #            "In late 90ies......IRD, Alain Fontneau",
                                 #            tags$br(),tags$br(),
                                 #            
                                 #            tags$br(),tags$br(),tags$h4("Sources"),
                                 #            tags$b("FIRMS / tuna RFMOs: "), tags$a(href="https://www", "IOTC page,")," with additional information from the ",tags$a(href="https://www", "FIRMS page."),
                                 #            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                                 #            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),
                                 #            
                                 #            tags$br(),tags$br(),tags$h4("Authors"),
                                 #            "Dr XXX BB, IRD",tags$br(),
                                 #            "DD BB FAO / IOTC",tags$br(),
                                 #            tags$br(),tags$br(),tags$h4("Contact: "),
                                 #            "julien barde at ird",tags$br(),tags$br(),
                                 #            tags$img(src = "https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg", width = "150px", height = "75px"),
                                 #            tags$img(src = "https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg", width = "150px", height = "75px")
                                 #          )
                        )
             )
  )
)

server <- function(input, output, session) {
  
  
  
  sql_query_metadata_plot1 <- eventReactive(input$submit, {
    paste0("Your zom is Zoom",zoom(),"   ;")
  },
  ignoreNULL = FALSE)
  
  # AND year IN ('",paste0(input$year,collapse="','"),"');")
  
  sql_query <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND species IN ({species_name*})
      AND country IN ({country_name*})
      AND year IN ({year_name*})
      GROUP BY species, country,geom_id, geom_wkt, geom , year
      ORDER BY species,country DESC",
      wkt = wkt(),
      species_name = input$species,
      country_name = input$country,
      year_name = year_name,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  sql_query_species_pie <- eventReactive(input$submit, {
    if(is.null(input$year)){year_name=target_year$year}else{year_name=input$year}
    query <- glue::glue_sql(
      "SELECT   geom_id, geom, species, SUM(value) as value, ST_asText(geom) AS geom_wkt FROM fact_tables.i6i7i8
      WHERE ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND country IN ({country_name*})
      AND year IN ({year_name*})
      GROUP BY species, geom_id, geom_wkt, geom
      ORDER BY value DESC",
      wkt = wkt(),
      species_name = input$species,
      country_name = input$country,
      year_name = year_name,
      .con = con)
  },
  ignoreNULL = FALSE)
  
  
  sql_query_metadata <- eventReactive(input$submit, {
    paste0("SELECT species, country, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, country, geom") 
  },
  ignoreNULL = FALSE)
  
  
  data <- eventReactive(input$submit, {
    # req(input$species)
    # req(input$country)
    # req(input$year)
    outp <- st_read(con, query = sql_query())
    outp
  },
  ignoreNULL = FALSE)
  
  
  metadata <- reactive({
    st_read(con, query = paste0("SELECT species, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, geom")) 
  })  
  
  data_pie_map <- reactive({
    # st_read(con, query = paste0("SELECT species, country, geom, sum(value) AS value FROM(SELECT geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText(('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'),4326)) AND species IN ('YFT') AND country IN ('EUESP', 'EUFRA', 'JPN', 'TWN') AND year IN ('2010') GROUP BY species, country,geom_id, geom_wkt, geom , year ORDER BY species,country DESC) AS foo GROUP BY species, country, geom"))
    st_read(con, query = paste0("SELECT species, country, geom, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, country, geom"))  %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  })
  
  data_pie_map_species <- reactive({
    st_read(con, query = paste0("SELECT species, geom, sum(value) AS value FROM(",sql_query_species_pie(),") AS foo GROUP BY species, geom"))  %>% spread(species, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(target_species$species)))))
  })
  
  data_time_serie <- reactive({
    st_read(con, query = paste0("SELECT species,to_date(year::varchar(4),'YYYY') AS  year, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY species, year")) 
  })
  
  data_pie_chart_country <- reactive({
    st_read(con, query = paste0("SELECT country, sum(value) AS value FROM(",sql_query(),") AS foo GROUP BY country ORDER BY country"))
  })
  
  
  centroid <- eventReactive(input$submit, {
    st_read(con, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;"))
  },
  ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   centroid(st_read(con, query = paste0("SELECT st_centroid(St_convexhull(st_collect(geom))) FROM  (",sql_query(),") AS foo;")))
  # },
  # ignoreInit = FALSE)
  
  # metadata <- eventReactive(input$submit, {
  #   st_read(con, query = sql_query_metadata())
  # },
  # ignoreNULL = FALSE)
  
  
  
  # data <- eventReactive(input$submit, {
  #   st_read(con, query = sql_query())
  # },
  # ignoreNULL = FALSE)
  
  # observeEvent(sql_query(), {
  #   data(st_read(con, query = sql_query()))
  # },
  # ignoreInit = FALSE)
  
  
  # observeEvent(data(), {
  #   # metadata(st_read(con, query = sql_query_metadata()))
  #   metadata(data()  %>% group_by(species,geom_wkt) %>% summarise(value = sum(value)))
  # },
  # ignoreInit = FALSE)
  # 
  # observeEvent(data(), {
  #   data_i11(data(data() %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))))
  # },
  # ignoreInit = FALSE)
  
  
  # data_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>% 
  #   #   mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  #   data()  %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country))))) %>% filter (total>mean(total))
  #   # data() %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(as.vector(input$country)))))
  #   # st_read(con, query = "SELECT ogc_fid, geom_id, geom, year, species, country, value, count,ST_asText(geom) AS geom_wkt FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('EU.ESP','JPN','TWN') AND year IN ('2014')") %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value)  %>%
  #   #   replace(is.na(.), 0) %>% mutate(total = rowSums(across(all_of(c("JPN","TWN"))))) %>% class()
  #     # mutate(total = rowSums(across(all_of(c("JPN","TWN")))))
  #      # rowwise()  %>% mutate(sumrow = as_data_frame(.)[,-c(1:3)])     replace(is.na(.), 0) %>%    all_of(input$country)))    mutate(sum = rowSums(across(where(is.numeric)))))
  # },
  # ignoreNULL = FALSE)
  
  
  
  
  
  # metadata_i11 <- eventReactive(input$submit, {
  #   # data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
  #   data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
  #   
  # },
  # ignoreNULL = FALSE)
  
  
  
  observeEvent(input$resetWkt, {
    wkt(global_wkt)
  },
  ignoreInit = TRUE)
  
  
  change <- reactive({
    unlist(strsplit(paste(c(input$species,input$year,input$country),collapse="|"),"|",fixed=TRUE))
  })
  
  
  observeEvent(input$species,{
    temp <- filters_combinations %>% filter(species %in% change()[1])
    updateSelectInput(session,"year",choices = unique(temp$year),selected=c(seq(min(temp$year):max(temp$year))+min(temp$year)-1))
    updateSelectInput(session,"country",choices = unique(temp$country),selected=unique(temp$country))
    
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
    # dplyr::select(species,country,value,geom_wkt)
    # dplyr::select(-c(geom))
    # as_data_frame(toto)[-c(1:3,ncol(as_data_frame(toto)))]
  }) 
  
  
  output$DTi11 <- renderDT({
    # this <- data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value))
    # toto <- st_read(con, query = "SELECT geom_id, geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, year FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('YFT') AND country IN ('EUESP','EUFRA','JPN','TWN') AND year IN ('2010','2011') GROUP BY species, country,geom_id, geom_wkt, geom , year ORDER BY species,country DESC") %>% group_by(species,country,geom_id) %>% summarise(value = sum(value))  %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
    
    data_pie_map_species()  %>% st_drop_geometry()
    
  }) 
  
  
  
  output$mymap <- renderLeaflet({
    
    
    # df <-st_read(con, query = "SELECT geom, year, species, country, value, ST_asText(geom) AS geom_wkt, ST_area(geom) AS area FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('EUESP','JPN','TWN') AND year IN ('2014') ORDER BY area DESC LIMIT 500;") %>% group_by(species,geom_wkt) %>% summarise(value = sum(value))
    # df <- metadata() %>% group_by(species,geom_wkt,area) %>% summarise(value = sum(value)) # %>% mutate(area=sf::st_area(st_as_sfc(geom_wkt)))  %>% filter(area>25)
    # df <- metadata() 
    # df <- data()  %>% group_by(species,geom_id) %>% summarise(value = sum(value))
    df <- metadata()
    # df <- st_read(con, query = query) %>% group_by(country,year,species,geom_wkt) %>% summarise(value = sum(value))  # %>% filter(species %in% input$species_i6i7i8)
    
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    # brewer.pal(7, "OrRd")
    # pal <- colorNumeric(palette = "YlGnBu",domain = df$value)
    # pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)
    # qpal <- colorQuantile("RdYlBu",df$value, n = 10)
    qpal <- colorQuantile(rev(viridis::viridis(10)),df$value, n=10)
    # qpal <- brewer.pal(n = 20, name = "RdBu")
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    # https://rstudio.github.io/leaflet/showhide.html
    mymap <- leaflet() %>% 
      addProviderTiles("Esri.OceanBasemap") %>% 
      # setView(lng = lon_centroid, lat =lat_centroid, zoom = 3
      # ) %>%
      clearBounds() %>%
      addPolygons(data = df,
                  label = ~value,
                  popup = ~paste0("Total catches for ",species," species in this square of the grid: ", round(value), " ton(t) et des brouettes"),
                  # popup = ~paste0("Captures de",species,": ", area, " tonnes(t) et des brouettes"),
                  # fillColor = ~pal_fun(value),
                  # fillColor = brewer.pal(n = 20, name = "RdBu"),
                  fillColor = ~qpal(value),
                  # color = ~pal(value)
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
      leaflet::addLegend("bottomright", pal = qpal, values = df$value,
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
    # output$plot_species<- renderPlot({ 
    df_i2 = st_read(con, query = paste0("SELECT species, count(species), sum(value) FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species ORDER BY count;")) # %>% filter (count>mean(count))
    
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
  output$map_i11 <- renderLeaflet({
    # toto <- data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%
    # toto <- data() %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country))))) #  %>% filter (total>mean(total))
    
    
    # test_data$grp = sapply(st_equals(test_data), max)
    # toto <- data() %>% group_by(species,country,geom_id) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>%  mutate(total = rowSums(across(any_of(as.vector(input$country)))))
    toto <- data_pie_map()
    
    # toto <- st_read(con, query = "SELECT geom, species, country, SUM(value) as value, ST_asText(geom) AS geom_wkt, ST_area(geom) AS area FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))',4326)) AND species IN ('SKJ') AND country IN ('OMN','NAM','BRA','AGO','CPV','USA','JPN','MEX','BRB','EUPRT','UNK','ECU','SHN','MYS','MAR','COL','MDV') AND year IN ('2013','2014','2015','2016','2017','2018','2019') GROUP BY area,species, country,geom_wkt, geom ORDER BY area,species,country DESC ;")  %>% 
    #   spread(country, value, fill=0)  %>% mutate(total = sum(across(any_of(c('OMN','NAM','BRA','AGO','CPV','USA','JPN','MEX','BRB','EUPRT','UNK','ECU','SHN','MYS','MAR','COL','MDV')))))  %>% filter (total>mean(total))
    # %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(as.vector(input$country)))))   %>% filter (total>mean(total))
    # toto <- df %>%  group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value, fill=0)  %>% mutate(total = rowSums(across(any_of(default_flag))))
    # toto <- data() %>% filter (year <= max(input$yearInterval) & year>=min(input$yearInterval)) %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    # toto <- df %>% group_by(species,country,geom_wkt) %>% summarise(value = sum(value)) %>% spread(country, value) 
    
    # centroid <-  st_convex_hull(st_union(toto)) %>%  st_centroid()
    lat_centroid <- st_coordinates(centroid())[2]
    lon_centroid <- st_coordinates(centroid())[1]
    
    # colors2 <- c("#3093e5","#3000e5", "#fcba50"," #dd0e34", "#4e9c1e")
    # qpal <- colorQuantile(rev(viridis::viridis(length(unique(toto$country)))),unique(toto$country), n=length(unique(toto$country)))
    # pal_fun <- brewer.pal(n = 30, name = "Dark2")
    
    # qpal <- colorQuantile(rev(viridis::viridis(10)),toto$total, n=10)
    # factpal <- colorFactor(topo.colors(ncol(dplyr::select(toto,-c(species,total)))),colnames(dplyr::select(toto,-c(species,total))))
    la_palette = palette3[names(palette3) %in% colnames(dplyr::select(toto,-c(species,total)))]
    
    # pal_fun <- colorQuantile("YlOrRd", NULL, n = length(unique(input$country)))
    # cocolor<-factor(toto$Species, levels=as.vector(input$country), labels=rainbow_hcl(length(as.vector(input$country))))
    
    
    # new_zoom <- input$map_i11_zoom
    
    # https://r-spatial.github.io/sf/articles/sf5.html
    map_i11 <-  leaflet() %>%  
      # map_i11 <-  leaflet(options = leafletOptions(zoomSnap=0.25)) %>%  
      # setView(lng = lon_centroid, lat = lat_centroid, zoom = 3) %>% 
      addProviderTiles("Esri.OceanBasemap", group = "background") %>%
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
  
  
  
  # observe({
  #   new_zoom <- input$map_i11_zoom
  #   req(input$map_i11_zoom)
  #   if(zoom()!=new_zoom & !is.null(input$map_i11_zoom)){
  #     zoom(new_zoom)
  #     #%>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>%  addProviderTiles("Esri.OceanBasemap", group = "background") %>%  clearBounds() %>%
  #     map_i11_proxy = leafletProxy("map_i11") %>% clearMinicharts() %>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>% 
  #       addMinicharts(lng = st_coordinates(st_centroid(data_pie_map(), crs = 4326))[, "X"],
  #                     lat = st_coordinates(st_centroid( data_pie_map(), crs = 4326))[, "Y"],
  #                     maxValues = max(data_pie_map()$total),
  #                     chartdata = dplyr::select(data_pie_map(),-c(species,total)) %>% st_drop_geometry(),type = "pie",
  #                     colorPalette = d3.schemeCategory10,
  #                     width = 10+(zoom()^2+200*(data_pie_map()$total/max(data_pie_map()$total))),
  #                     legend = TRUE, legendPosition = "bottomright")
  #     
  #     
  #   }
  #   
  # })
  
  
  observeEvent(input$refresh_map,{
    new_zoom <- input$map_i11_zoom
    req(input$map_i11_zoom)
    if(zoom()!=new_zoom & !is.null(input$map_i11_zoom)){
      la_palette = palette3[names(palette3) %in% colnames(dplyr::select(data_pie_map(),-c(species,total)))]
      zoom(new_zoom)
      lat_centroid <-input$map_i11_center[2]
      lon_centroid <- input$map_i11_center[1]
      #%>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>%  addProviderTiles("Esri.OceanBasemap", group = "background") %>%  clearBounds() %>%
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
    # output$pie_map_i11 <- renderPlot({
    
    # df_i11_map <- data_i11() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
    # metadata_i11 <- data() %>% group_by(country) %>% summarise(value = sum(value))  %>% arrange(desc(value)) # %>% top_n(3)
    metadata_i11 <- data_pie_chart_country() 
    # df_i11_map <- as_data_frame(metadata_i11())  # %>% top_n(3)
    df_i11_map <- as_tibble(metadata_i11)  # %>% top_n(3)
    
    la_palette = palette3[names(palette3) %in% unique(df_i11_map$country)]
    
    
    # # # Basic piechart
    # i11_map <-   ggplot(df_i11_map, aes(x="", y=value, fill=country)) +
    #   geom_bar(stat="identity", width=1) +
    #   coord_polar("y", start=0) + 
    # scale_fill_manual(values = la_palette)
    # ggplotly(i11_map)
    # #   theme(axis.text.x = element_text(angle = 90))
    
    
    
    
    fig <- plot_ly(df_i11_map, labels = ~country, values = ~value, type = 'pie',
                   marker = list(colors = la_palette, line = list(color = '#FFFFFF', width = 1), sort = FALSE),
                   showlegend = TRUE)
    fig <- fig %>% layout(title = 'Tuna catches by country for selected species, area and period of time',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
  })
  
  
  output$plot1_streamgraph <- renderDygraph({
    # output$plot1_streamgraph <- renderPlot({
    
    # df_i1 = st_read(con, query = sql_query_metadata_plot1()) %>% group_by(species,year) %>% summarise(value = sum(value))  %>% arrange(desc(value)) %>% filter (value>mean(value)) # %>% top_n(3)
    # df_i1 = data() %>% group_by(species,year) %>% summarise(value = sum(value))  %>% arrange(desc(value))  %>% filter (value>mean(value)) # %>% top_n(3)
    df_i1 = data_time_serie() # %>%  arrange(desc(value))  %>% filter (value>mean(value)) # %>% top_n(3)
    # df_i1 = st_read(con, query = paste0("SELECT species, year, count(species), sum(value) AS value FROM fact_tables.i6i7i8 WHERE ST_Within(geom,ST_GeomFromText('",wkt(),"',4326)) GROUP BY species,year ORDER BY count;")) %>% filter (count>mean(count))
    
    # value=as.vector(as.integer(df_i1$value))
    # g1 = ggplot(df_i1, aes(x = year, y = value, colour = species)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    #   # geom_bar(position="stack", stat="identity") +
    #   ylab("Catches in Tons") + xlab("Date") + theme_bw() +
    #   scale_colour_manual(values=c(value)) +
    #   # scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "kT")}) +
    #   theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
    #         plot.margin = margin(5, 12, 5, 5))
    # 
    
    # https://rstudio.github.io/dygraphs/gallery-timezones.html
    # create time series object
    df_i1 <- as_tibble(df_i1)  # %>% top_n(3)
    
    tuna_catches_timeSeries <- xts(x = df_i1$value, order.by = df_i1$year)
    
    # create the area chart
    g1 <- dygraph(tuna_catches_timeSeries) %>% dyOptions( fillGraph=TRUE )
    
    # create a basic interactive element
    # g1 <- dygraph(discharge_timeSeries)  %>% dyRangeSelector()
    
    g1
    
    
    
    # 
  })
  
  
  output$plot11 <- renderImage({
    # https://semba-blog.netlify.app/06/13/2020/plots-in-interactive-maps-with-r/
    df_i11_filtered <- as(data(), "Spatial")
    
    i11 <- Atlas_i11_CatchesByCountry(df=df_i11_filtered,
                                      geomIdAttributeName="geom_id",
                                      countryAttributeName="country",
                                      speciesAttributeName="species",
                                      valueAttributeName="value",
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
      addProviderTiles("Esri.OceanBasemap", group = "background") %>%
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
  
  
  # 
  # observeEvent(input$refresh_map,{
  #   new_zoom <- input$map_i11_zoom
  #   req(input$map_i11_zoom)
  #   if(zoom()!=new_zoom & !is.null(input$map_i11_zoom)){
  #     la_palette = palette3[names(palette3) %in% colnames(dplyr::select(data_pie_map(),-c(species,total)))]
  #     zoom(new_zoom)
  #     lat_centroid <-input$map_i11_center[2]
  #     lon_centroid <- input$map_i11_center[1]
  #     #%>% setView(lng = lon_centroid, lat = lat_centroid, zoom = zoom()) %>%  addProviderTiles("Esri.OceanBasemap", group = "background") %>%  clearBounds() %>%
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
  # 
  
  
  
  
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)



