
renv::restore(exclude = c("geojsonio", "protolite", "geojson", "magick"))
source('install.R')

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
####################################################################################################################################################################################################################################
require(dygraphs)
require(shiny)
require(DBI)
require(plotly)
require(leaflet.minicharts)
require(ncdf4)
require(pool)
library(glue)
library(dplyr)
library(leaflet)
library(DT)
require(bslib)
require(gridlayout)
require(here)
require(RColorBrewer)

connect_to_db <- function() {
  
try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))
  
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

target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.i6i7i8 ORDER BY dataset;")
target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.i6i7i8 ORDER BY fishing_fleet;") %>% dplyr::filter(!is.na(fishing_fleet))
target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.i6i7i8 ORDER BY gridtype;")

default_dataset <- ifelse('global_catch_firms_level0' %in%target_dataset$dataset, "global_catch_firms_level0", target_dataset[[1]][1])
default_flag <- ifelse('EUFRA' %in%target_flag, "EUFRA", target_flag[[1]][1])
default_species <- dbGetQuery(pool, paste0("SELECT DISTINCT(species) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' ORDER BY species;"))[[1]][1]
default_gridtype <- dbGetQuery(pool, paste0("SELECT DISTINCT(gridtype) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND species = '", default_species, "' LIMIT 1;"))
default_year <- dbGetQuery(pool, paste0("SELECT DISTINCT(year) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND gridtype = '", default_gridtype, "' AND species = '", default_species, "' LIMIT 1;"))

filters_combinations <- dbGetQuery(pool, "SELECT dataset, gridtype, species, year, fishing_fleet FROM  public.i6i7i8 GROUP BY dataset, gridtype, species, year, fishing_fleet;")
####################################################################################################################################################################################################################################

default_gridtype <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  head(1) %>% 
  pull(gridtype)

default_species <- filters_combinations %>% dplyr::filter(dataset == default_dataset) %>% 
  dplyr::filter(gridtype == default_gridtype) %>% 
  head(1) %>% 
  pull(species)

default_flag <- unique(filters_combinations$fishing_fleet)
variable_to_display <-c("species","fishing_fleet","measurement_value", "gear_type")           


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
source(here::here('modules/categoryGlobalPieChart.R'))
source(here::here('modules/pieMapTimeSeriesUI.R'))



targettes <- list(
  species = target_species,        # Defined elsewhere, as shown above
  fishing_fleet = target_flag  # Defined elsewhere, as shown above
)
getTarget <- function(category) {
  target <- targettes[[category]]
  
  return(target)
}
