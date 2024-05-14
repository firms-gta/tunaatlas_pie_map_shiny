# This file launched with runApp
if(!require(renv)){
  install.packages('renv', repos='https://cran.r-project.org/')
  require(renv)
}

renv::restore(exclude = c("shinyuieditor"))
require(here)
source(here::here('install.R'))

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
# source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
load_libraries <- function() {
  libraries <- c(
    "dygraphs", "shiny", "DBI", "plotly", "leaflet.minicharts", "ncdf4", 
    "pool", "glue", "dplyr", "leaflet", "DT", "bslib", "gridlayout", 
    "here", "RColorBrewer", "shinyjs", "rlang", "rmarkdown", "sf"
  )
  lapply(libraries, require, character.only = TRUE)
}
load_libraries()

connect_to_db <- function() {
  
  dotenv::load_dot_env(here::here("connection_tunaatlas_inv.txt"))
  
  print("dotenv.ok")
  
  db_host <- Sys.getenv("DB_HOST")
  db_port <- as.integer(Sys.getenv("DB_PORT"))
  db_name <- Sys.getenv("DB_NAME")
  db_user <- Sys.getenv("DB_USER_READONLY")
  db_password <- Sys.getenv("DB_PASSWORD")
  
  dbPool(RPostgreSQL::PostgreSQL(),
         host = db_host,
         port = db_port,
         dbname = db_name,
         user = db_user,
         password = db_password)
}

pool <- connect_to_db()
####################################################################################################################################################################################################################################
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 

target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")
target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.shinycatch ORDER BY fishing_fleet;")
target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.shinycatch ORDER BY gridtype;")
target_gear_type <- dbGetQuery(pool, "SELECT DISTINCT(gear_type) FROM public.shinycatch ORDER BY gear_type;")
target_measurement_unit <- dbGetQuery(pool, "SELECT DISTINCT(measurement_unit) FROM public.shinycatch ORDER BY measurement_unit;")
target_fishing_mode <- dbGetQuery(pool, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

default_dataset <- ifelse('global_catch_firms_level0' %in%target_dataset$dataset, "global_catch_firms_level0", target_dataset[[1]][1])

filters_combinations <- dbGetQuery(pool, "SELECT dataset, gear_type, gridtype, species, year, fishing_fleet, fishing_mode, measurement_unit FROM  public.shinycatch GROUP BY dataset,gear_type, gridtype, species, year, fishing_fleet, fishing_mode, measurement_unit;")
####################################################################################################################################################################################################################################

default_gridtype <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  head(1) %>% 
  pull(gridtype)

default_measurement_unit <- "t"

default_species <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  dplyr::filter(gridtype == default_gridtype) %>% dplyr::filter(measurement_unit == default_measurement_unit)%>% 
  head(5) %>% 
  pull(species)


filtered_combinations_default <-filters_combinations%>% dplyr::filter(dataset == default_dataset) %>% dplyr::filter(measurement_unit == default_measurement_unit)%>% 
  dplyr::filter(gridtype == default_gridtype) 

default_flag <- unique(filtered_combinations_default$fishing_fleet)
default_gear_type <- unique(filtered_combinations_default$gear_type)
default_fishing_mode <- unique(filtered_combinations_default$fishing_mode)

variable_to_display <-c("species","fishing_fleet","measurement_value", "gear_type", "fishing_mode")           

analysis_options <- list(
  list(title = "Species Analysis", id = "species_analysis"),
  list(title = "Fishing Fleet Analysis", id = "fleet_analysis"),
  list(title = "Gear Type Analysis", id = "gear_analysis"),
  list(title = "Fishing Mode Analysis", id = "fishing_mode_analysis")
)

####################################################################################################################################################################################################################################
load_ui_modules <- function() {
  ui_files <- c(
    'tab_panels/geographic_catches_ui.R', 
    'tab_panels/main_panel_ui.R', 
    'tab_panels/geographic_catches_by_variable_ui.R',
    'tab_panels/ggplot_indicator_11_ui.R',
    'tab_panels/additional_info_ui.R', 
    'tab_panels/filterUI.R',
    'tab_panels/data_explorer_overview_ui.R', 
    'tab_panels/total_catch_plot.R',
    'tab_panels/sidebar_ui.R', 
    'tab_panels/mapCatchesmodules.R',
    'tab_panels/create_logo_panel.R', 
    'tab_panels/dataset_choice.R',
    'tab_panels/sqlqueriesui.R', 
    'tab_panels/data_explorer_i11_ui.R', 
    'tab_panels/more_about.R',
    'rmd/rendering_rmd_files_to_html.R',
    'rmd/rendering_rmd_files_to_html.R',
    'modules/generateRmdNavMenu.R',
    'modules/categoryGlobalPieChart.R', 
    'modules/pieMapTimeSeriesUI.R',
    'R/get_html_title.R', 
    'R/getPalette.R',
    'R/palette_settings.R'
    
  )
  lapply(ui_files, function(file) source(here::here(file)))
}


# Préparation de la liste targetVariables
targetVariables <- list(
  species = target_species,
  fishing_fleet = target_flag,
  gear_type = target_gear_type, 
  fishing_mode = target_fishing_mode
)

# Initialisation des palettes avec une graine fixe pour assurer la reproductibilité
palettes <- initialiserPalettes(targetVariables, seed=2643598) 

targettes <- list(
  species = target_species,        
  fishing_fleet = target_flag, 
  gear_type = target_gear_type, fishing_mode = target_fishing_mode
)
getTarget <- function(category) {
  target <- targettes[[category]]
  
  return(target)
}


createSQLQuery <- function(dataset_name = default_dataset,
                           species_name = default_species,
                           fishing_fleet_name = default_flag,
                           gear_type_name = default_gear_type,
                           selected_years = target_year$year,
                           measurement_unit_name = default_measurement_unit,
                           fishing_mode_name = default_fishing_mode,
                           wkt = global_wkt,
                           con = pool) {
  
  query <- glue::glue_sql(
    "SELECT   geom_id, geom, species,gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode,
  ST_asText(geom) AS geom_wkt, year FROM public.shinycatch
      WHERE dataset IN ({dataset_name})
      AND ST_Within(geom,ST_GeomFromText(({wkt*}),4326))
      AND fishing_fleet IN ({fishing_fleet_name*})
      AND species IN ({species_name*})
      AND gear_type IN ({gear_type_name*})
      AND year IN ({selected_years*})
      AND measurement_unit IN ({measurement_unit_name*})
      AND fishing_mode IN ({fishing_mode_name*})
      GROUP BY species, fishing_fleet,geom_id, geom_wkt, geom , year, gear_type, fishing_mode, measurement_unit
      ORDER BY species,fishing_fleet DESC", 
    .con = pool)
  
  return(query)
}

sql_query_init <- createSQLQuery()

initialize_data_and_plots <- function(pool, sql_query_init) {
  data_init <- st_read(pool, query = sql_query_init)
  
  data_without_geom <- as.data.frame(data_init)
  data_without_geom$geom_wkt <- NULL
  df <- data_without_geom %>%
    dplyr::group_by(.data[["fishing_fleet"]], year) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>% ungroup()
  
  top_n_groups <- df %>%
    dplyr::group_by(.data[["fishing_fleet"]]) %>%
    dplyr::summarise(total = sum(measurement_value)) %>%
    dplyr::top_n(5, total) %>%
    pull(.data[["fishing_fleet"]])
  
  df <- df %>%
    dplyr::mutate(!!sym("fishing_fleet") := if_else(.data[["fishing_fleet"]] %in% top_n_groups, as.character(.data[["fishing_fleet"]]), "Other")) %>%
    dplyr::group_by(.data[["fishing_fleet"]], year) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
  
  plot_init <- ggplot(df, aes_string(x = "year", y = "measurement_value", group = "fishing_fleet", color = "fishing_fleet")) +
    geom_line() + labs(title = "Yearly Data", x = "Year", y = "Measurement Value")
  
  png("tab_panels/plot_init.png")
  print(plot_init)
  dev.off()
  
  a <- st_read(pool, query = paste0("SELECT geom, sum(measurement_value) AS measurement_value FROM(", sql_query_init, ") AS foo GROUP BY geom"))
  
  qpal <- colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n=10)
  tmap_mode("plot")
  map_init <- leaflet() %>% 
    addProviderTiles("Esri.NatGeoWorldMap") %>% 
    clearBounds() %>%
    addPolygons(data = a,
                label = ~measurement_value,
                popup = ~paste0("Total catches for the selected criteria in this square of the grid: ", round(measurement_value), " tons (t) et des brouettes"),
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
    leaflet::addLegend("bottomright", pal = qpal, values = a$measurement_value,
                       title = "Quantile of the grid for the total catches",
                       labFormat = labelFormat(prefix = "MT "),
                       opacity = 1
    )
}
initialize_data_and_plots(pool, sql_query_init)

#---------------------------------------------------------------------------------------
source(here::here("ui.R"))
source(here::here("server.R"))
