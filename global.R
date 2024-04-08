
renv::restore(exclude = c("geojsonio", "protolite", "geojson", "magick"))
source('install.R')

####################################################################################################################################################################################################################################
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
# source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/wkt2spdf.R")
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
require(shinyjs)

connect_to_db <- function() {
  # if (is.null(pool) || !dbIsValid(pool)) {
    
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
source(here::here('tab_panels/geographic_catches_by_variable_ui.R'))
source(here::here('tab_panels/ggplot_indicator_11_ui.R'))
source(here::here('tab_panels/additional_info_ui.R'))
source(here::here('tab_panels/filterUI.R'))
source(here::here('tab_panels/data_explorer_overview_ui.R'))
source(here::here('tab_panels/total_catch_plot.R'))
source(here::here('tab_panels/sidebar_ui.R'))
source(here::here('tab_panels/mapCatchesmodules.R'))
source(here::here('modules/categoryGlobalPieChart.R'))
source(here::here('modules/pieMapTimeSeriesUI.R'))
source(here::here('tab_panels/create_logo_panel.R'))
source(here::here("R/palette_settings.R"))
source(here::here("tab_panels/dataset_choice.R"))
source(here::here("tab_panels/sqlqueriesui.R"))
source(here::here("tab_panels/data_explorer_i11_ui.R"))
source(here::here("rmd/rendering_rmd_files_to_html.R"))

targettes <- list(
  species = target_species,        
  fishing_fleet = target_flag  
)
getTarget <- function(category) {
  target <- targettes[[category]]
  
  return(target)
}


list_markdown_path <- c("rmd/Authors.html", "rmd/Datasets.html", "rmd/Fundings.html")
