# Restore the renv environment, excluding shinyuieditor
# renv::restore(exclude = c("shinyuieditor"))
# try(pool::dbDisconnect(pool))
# Source install script
require(here)
source(here::here('install.R'))
# Log the loading of libraries
flog.info("All libraries loaded successfully.")
#create default_dataset
source(here::here("R/create_or_load_default_dataset.R"))
flog.info("Sourced create or load defautl dataset")

source(here::here("R/load_ui_modules.R"))
load_ui_modules()
flog.info("Sourced loading ui modules dataset")

variable <- c("source_authority",
              "fishing_fleet",
              "species_group", 
              "Gear",
              "gear_type", 
              "species_name",
              "fishing_mode", 
              "measurement_unit",
              "gridtype",
              "species"
)

flog.info(sprintf("Variables: %s", paste0(variable)))

source(here::here("R/palette_settings.R"))
source(here::here("R/generate_dimensions_palettes.R"))

# Load functions from external sources
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
flog.info("Functions loaded")

getTarget <- function(category) {
  target <- targettes[[category]]
  return(target)
}
# Adding resource path to display html -------------------------------------
addResourcePath("www", here::here("www"))

sql_query_init <- readRDS(here::here("tab_panels/sql_query_init.rds"))
flog.info("SQl query loaded")

map_init <- read_html(here::here("www/map_init.html"))
flog.info("Map init loaded")
# Générer les options d'analyse

# source(here::here("tab_panels/sidebar_ui_with_variable_to_display.R"))
source(here::here("tab_panels/sidebar_ui.R"))

# Générer targetVariables et targettes

# if(exists("debug_mode") && debug_mode){
# default_dataset_preloaded <- readRDS(here::here("data/default_dataset_preloaded.rds"))
# } else {
#   default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
# }
# Rprofmem("memory_profile.txt")
source(here::here("R/initialize_reactive_values.R")) # and database connection
# Rprofmem(NULL)
# Log that the UI and server files have been sourced successfully
flog.info("Global.R file loaded")

