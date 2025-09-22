if(requireNamespace("reactlog")){ # utiliser unqiuement dans branche dev qui est la seule dont le renv.lock contient reactlog
  options(
  shiny.sanitize.errors = FALSE,   # montre les vrais messages dans l’UI
  shiny.fullstacktrace = TRUE,     # stacktrace complète
  shiny.trace = TRUE               # trace les invalidations dans la console
)
reactlog::reactlog_enable()        # graphe des dépendances
} 
# Restore the renv environment, excluding shinyuieditor
# renv::restore(exclude = c("shinyuieditor"), prompt= FALSE)
# try(pool::dbDisconnect(pool))
# Source install script
before_package <-Sys.time()
print(before_package)
require(here)
# source(here::here('install.R'))
require(sf)
sf::sf_use_s2(FALSE)
# tempory add
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
unique_packages <- c("xts", "ggplot2",
  "RPostgreSQL", "here", "tools", "sf", "dplyr", "qs", 
  "futile.logger", "shinyjs", "tidyr", "bslib", 
  "shiny", "readr", "glue", "stringr", "knitr", 
  "DT", "viridis", "leaflet", "geojsonsf", "scales", 
  "dotenv", "zoo", "RColorBrewer", "shinycssloaders", "data.table", "htmlwidgets",
  "xml2", "gridlayout", "dygraphs", "plotly","leaflet.extras","leaflet.minicharts", 
  "pool", "jsonlite", "flextable", "cowplot"
)
require(futile.logger)

lapply(unique_packages, function(pkg) {
  start_time <- Sys.time()  # Record start time
  flog.info("Loading package: %s", pkg)
  
  library(pkg, character.only = TRUE)
  
  end_time <- Sys.time()  # Record end time
  load_duration <- end_time - start_time  # Calculate duration
  
  flog.info("Loaded package: %s in %s seconds", pkg, load_duration)
})

# Log total loading time
flog.info("All libraries loaded successfully in %s seconds", Sys.time() - before_package)
# Log the loading of libraries
flog.info("All libraries loaded successfully.")
#create default_dataset
source(here::here("update_data.R"))
source(here::here("create_or_load_default_dataset.R"))

source(here::here("modules/load_ui_modules.R"))
source(here::here("modules/report_module.R"))
source(here::here("modules/db_connect.R"))
load_ui_modules()
flog.info("Sourced loading ui modules dataset")

flog.info(sprintf("Variables: %s", paste0(variable)))

# Load functions from external sources
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
flog.info("Functions loaded")

getTarget <- function(category) {
  target <- targettes[[category]]
  return(target)
}
# Adding resource path to display html -------------------------------------
# addResourcePath("www", here::here("www"))

# sql_query_init <- qs::qread(here::here("tab_panels/sql_query_init.qs"))
flog.info("SQl query loaded")

# map_init <- read_html(here::here("www/map_init.html"))
flog.info("Map init loaded")
# Générer les options d'analyse

# source(here::here("tab_panels/sidebar_ui_with_variable_to_display.R"))
source(here::here("tab_panels/sidebar_ui.R"))

# Générer targetVariables et targettes

# if(exists("debug_mode") && debug_mode){
# default_dataset_preloaded <- qs::qread(here::here("data/default_dataset_preloaded.qs"))
# } else {
#   default_dataset_preloaded <- qs::qread(here::here("data/datasf.qs"))
# }
# source(here::here("Markdown/reportmarkdown.R"))
source(here::here("modules/initialize_reactive_values.R"))
source(here::here("R/initialiserPalettes.R"))
source(here::here("global/generate_dimensions_palettes.R"))
source(here::here("modules/categoryGlobalChart.R"))
flog.info("Palette ok ")
source(here::here("R/get_html_title.R"))
source(here::here("R/getPalette.R")) # do not knwo why we should run it as it is supposed to be ran after global.R but yet it is not
addResourcePath("www", here::here("www"))
more_about = function(){
  generateRmdNavMenu("rmd_docs", nav_bar_menu_html)
}
flog.info("Menu generated ")
outputmoreabout <- more_about()
source(here::here("R/data_loading.R"))
# Rprofmem("memory_profile.txt")
# Rprofmem(NULL)
# Log that the UI and server files have been sourced successfully
flog.info(paste0("Global.R file loaded in ",  Sys.time() - before_package, " seconds"))