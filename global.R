
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
library(shiny)
library(DBI)
library(glue)
library(dplyr)
library(leaflet)
library(DT)

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


# getSpeciesChoices <- function(dataset, gridtype) {
#   query <- sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", dataset, gridtype)
#   species <- dbGetQuery(pool, query)$species
#   return(species)
# }
# 
# # Function to get fleets based on dataset and gridtype
# getFleetChoices <- function(dataset, gridtype) {
#   query <- sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", dataset, gridtype)
#   fleets <- dbGetQuery(pool, query)$fishing_fleet
#   return(fleets)
# }
# 
# 
# 
# getDatasetChoices <- function(pool) {
#   query <- "SELECT DISTINCT dataset FROM public.i6i7i8 ORDER BY dataset;"
#   return(dbGetQuery(pool, query)$dataset)
# }
# 
# getGridtypeChoices <- function(pool, selectedDataset) {
#   # Cette fonction peut être améliorée pour filtrer les choix de gridtype basés sur le dataset sélectionné
#   query <- sprintf("SELECT DISTINCT gridtype FROM public.i6i7i8 WHERE dataset = '%s' ORDER BY gridtype;", selectedDataset)
#   return(dbGetQuery(pool, query)$gridtype)
# }
# 
# getSpeciesChoices <- function(pool, selectedDataset, selectedGridtype) {
#   query <- sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", selectedDataset, selectedGridtype)
#   choices <- dbGetQuery(pool, query)$species
#   return(c("All" = "all", choices))
# }
# 
# getFleetChoices <- function(pool, selectedDataset, selectedGridtype) {
#   query <- sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", selectedDataset, selectedGridtype)
#   choices <- dbGetQuery(pool, query)$fishing_fleet
#   return(c("All" = "all", choices))
# }

####################################################################################################################################################################################################################################
pool <- connect_to_db()
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1) 

target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.i6i7i8 ORDER BY dataset;")
target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.i6i7i8 ORDER BY species;")
target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.i6i7i8 ORDER BY year;")
target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.i6i7i8 ORDER BY fishing_fleet;") %>% dplyr::filter(!is.na(fishing_fleet))
target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.i6i7i8 ORDER BY gridtype;")

default_dataset <- ifelse('global_catch_firms_level0' %in%target_dataset, "global_catch_firms_level0", target_dataset[[1]][1])
default_flag <- ifelse('EUFRA' %in%target_flag, "EUFRA", target_flag[[1]][1])
default_species <- dbGetQuery(pool, paste0("SELECT DISTINCT(species) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' ORDER BY species;"))[[1]][1]
default_gridtype <- dbGetQuery(pool, paste0("SELECT DISTINCT(gridtype) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND species = '", default_species, "' LIMIT 1;"))
default_year <- dbGetQuery(pool, paste0("SELECT DISTINCT(year) FROM public.i6i7i8 WHERE dataset = '", default_dataset, "' AND gridtype = '", default_gridtype, "' AND species = '", default_species, "' LIMIT 1;"))

filters_combinations <- dbGetQuery(pool, "SELECT dataset, gridtype, species, year, fishing_fleet FROM  public.i6i7i8 GROUP BY dataset, gridtype, species, year, fishing_fleet;")
onStop(function() {
  poolClose(pool)
})
require(bslib)

create_logo_panel <- function() {
  div(
    tags$a(href='https://www.ird.fr/', 
           tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg', 
                    height='89px', width='108px', style="margin: 10px;")),
    style="text-align: start;"
  )
}

