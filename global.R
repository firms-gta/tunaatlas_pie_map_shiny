
renv::restore()
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


getSpeciesChoices <- function(dataset, gridtype) {
  query <- sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", dataset, gridtype)
  species <- dbGetQuery(pool, query)$species
  return(species)
}

# Function to get fleets based on dataset and gridtype
getFleetChoices <- function(dataset, gridtype) {
  query <- sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", dataset, gridtype)
  fleets <- dbGetQuery(pool, query)$fishing_fleet
  return(fleets)
}



getDatasetChoices <- function(pool) {
  query <- "SELECT DISTINCT dataset FROM public.i6i7i8 ORDER BY dataset;"
  return(dbGetQuery(pool, query)$dataset)
}

getGridtypeChoices <- function(pool, selectedDataset) {
  # Cette fonction peut être améliorée pour filtrer les choix de gridtype basés sur le dataset sélectionné
  query <- sprintf("SELECT DISTINCT gridtype FROM public.i6i7i8 WHERE dataset = '%s' ORDER BY gridtype;", selectedDataset)
  return(dbGetQuery(pool, query)$gridtype)
}

getSpeciesChoices <- function(pool, selectedDataset, selectedGridtype) {
  query <- sprintf("SELECT DISTINCT species FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY species;", selectedDataset, selectedGridtype)
  choices <- dbGetQuery(pool, query)$species
  return(c("All" = "all", choices))
}

getFleetChoices <- function(pool, selectedDataset, selectedGridtype) {
  query <- sprintf("SELECT DISTINCT fishing_fleet FROM public.i6i7i8 WHERE dataset = '%s' AND gridtype = '%s' ORDER BY fishing_fleet;", selectedDataset, selectedGridtype)
  choices <- dbGetQuery(pool, query)$fishing_fleet
  return(c("All" = "all", choices))
}



