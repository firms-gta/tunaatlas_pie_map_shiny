# Restore the renv environment, excluding shinyuieditor
# renv::restore(exclude = c("shinyuieditor"))
# try(pool::dbDisconnect(pool))
# Source install script
source(here::here('install.R'))
# Load functions from external sources
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")
flog.info(sprintf("current env:",current_env()))
# Load UI modules
source(here::here("R/load_ui_modules.R"))
load_ui_modules()

# Log the loading of libraries
flog.info("All libraries loaded successfully.")

# Load environment variables from file
try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))

# Create database connection pool
# Log environment variables
db_host <- Sys.getenv("DB_HOST")
db_port <- as.integer(Sys.getenv("DB_PORT"))
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER")
db_user_readonly <- Sys.getenv("DB_USER_READONLY")
db_password <- Sys.getenv("DB_PASSWORD")

flog.info("Attempting to connect to the database with the following parameters:")
flog.info("Host: %s", db_host)
flog.info("Port: %d", db_port)
flog.info("Database Name: %s", db_name)
flog.info("User: %s", db_user)
flog.info("User readonly: %s", db_user_readonly)

# Create database connection pool
tryCatch({
  pool <- dbPool(RPostgreSQL::PostgreSQL(),
                 host = db_host,
                 port = db_port,
                 dbname = db_name,
                 user = db_user_readonly,
                 password = db_password)
  flog.info("Database connection pool created successfully.")
}, error = function(e) {
  flog.error("Failed to create database connection pool: %s", e$message)
})

# Log the successful creation of the connection pool
flog.info("Database connection pool to '%s' has been created successfully.", db_name)
if(!exists("default_dataset") | exists("debug")){
  
  flog.info("Loading data ")
  # Read the DOI CSV file
  
  DOI <- read_csv('data/DOI.csv')
  # DOI$Filename <- "global_catch_tunaatlasird_level2.csv"
  load_data <- function(DOI) {
    loaded_data <- list()
    
    for (filename in DOI$Filename) {
      flog.info("Loading dataset: %s", filename)
      
      base_filename <- tools::file_path_sans_ext(filename) # Remove any existing extension
      csv_file_path <- file.path('data', paste0(base_filename, '.csv'))
      rds_file_path <- file.path('data', paste0(base_filename, '.rds'))
      
      if (file.exists(csv_file_path)) {
        # Read CSV file with specific column type for gear_type
        loaded_data[[base_filename]] <- read_csv(csv_file_path, 
                                                 col_types = cols(gear_type = col_character()))
        assign(base_filename, as.data.frame(loaded_data[[base_filename]]), envir = .GlobalEnv)
      } else if (file.exists(rds_file_path)) {
        loaded_data[[base_filename]] <- readRDS(rds_file_path)
        # Ensure gear_type is character after reading from RDS
        if ("gear_type" %in% names(loaded_data[[base_filename]])) {
          loaded_data[[base_filename]]$gear_type <- as.character(loaded_data[[base_filename]]$gear_type)
        }
        assign(base_filename, loaded_data[[base_filename]], envir = .GlobalEnv)
      } else {
        warning(paste('File not found:', csv_file_path, 'or', rds_file_path))
      }
    }
  }

  
load_data(DOI)
  
flog.info("Reactive values initialized")


# Define function to get target values based on category
getTarget <- function(category) {
  target <- targettes[[category]]
  return(target)
}

source(here::here("R/initialize_reactive_values.R"))

# source(here::here("R/initialize_data_and_plots.R"))
default_dataset <- as.data.frame(default_dataset)


# Log the initialization of palettes
flog.info("Color palettes initialized.")

}

source(here::here("R/generate_dimensions_palettes.R"))

# Adding resource path to display html -------------------------------------
addResourcePath("www", here::here("www"))

sql_query_init <- readRDS(here::here("tab_panels/sql_query_init.rds"))
flog.info("SQl query loaded")

map_init <- read_html(here::here("www/map_init.html"))
flog.info("Map init loaded")
# Générer les options d'analyse

source(here::here("tab_panels/sidebar_ui_with_variable_to_display.R"))
source(here::here("tab_panels/sidebar_ui.R"))

# Générer targetVariables et targettes

# if(exists("debug_mode") && debug_mode){
# default_dataset_preloaded <- readRDS(here::here("data/default_dataset_preloaded.rds"))
# } else {
#   default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
# }

source(here::here("R/data_loading.R"))

# Log that the UI and server files have been sourced successfully
flog.info("Global.R file loaded")


# geographic_catches_ui <- function(variable_to_display) {
#   nav_panel(
#     title = "General overview", value = "generaloverview",
#     grid_container(
#       layout = c(
#         "mapcatches   by_month  ",
#         "plot_catches by_month"
#       ),
#       row_sizes = c("0.8fr", "1.2fr"),
#       col_sizes = c("1.4fr", "0.6fr"),
#       gap_size = "10px",
#       grid_card(
#         area = "mapcatches",
#         card_body(mapCatchesUI("total_catch"))
#       ),
#       grid_card(area = "plot_catches", card_body(plotTotalCatchesUI("catch_by_year"))),
#       grid_card(area = "by_month", card_body(catches_by_variable_moduleUI("catches_by_variable_month", variable_to_display)))
#     )
#   )
# }
