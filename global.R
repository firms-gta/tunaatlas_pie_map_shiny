# Restore the renv environment, excluding shinyuieditor
# renv::restore(exclude = c("shinyuieditor"))

# Source install script
source(here::here('install.R'))

# Load functions from external sources
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")

# Log the loading of libraries
flog.info("All libraries loaded successfully.")

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

source("~/tunaatlas_pie_map_shiny/R/initialize_reactive_values.R")
# Query distinct values from the database for filters

# saveRDS(list(target_dataset = target_dataset, 
#              target_species = target_species, 
#              target_year = target_year, 
#              target_flag = target_flag, 
#              target_gridtype = target_gridtype, 
#              target_gear_type = target_gear_type, 
#              target_measurement_unit = target_measurement_unit, 
#              target_fishing_mode = target_fishing_mode), 
#         "data/target.rds")

# Log the successful retrieval of filter options
flog.info("Filter options retrieved from database.")

source(here::here("R/initialize_data_and_plots.R"))

default_dataset <- as.data.frame(default_dataset)

# Create target_* variables for each non-numeric column
for (col in variable_to_display) {
  assign(paste0("target_", col), unique(default_dataset[[col]]))
}

# Fonction pour générer les titres et IDs d'analyse
generate_analysis_option <- function(variable) {
  title <- paste0(toupper(substring(variable, 1, 1)), substring(variable, 2), " Analysis")
  id <- paste0(variable, "_analysis")
  list(title = title, id = id)
}

# Générer les options d'analyse
analysis_options <- lapply(variable_to_display, generate_analysis_option)
load_ui_modules <- function() {
  ui_files <- c(
    "R/data_loading.R", 
    'R/get_html_title.R',
    'R/getPalette.R',
    'R/palette_settings.R', 
    "tab_panels/sidebar_ui_with_variable_to_display.R"
  )
  lapply(ui_files, function(file) {
    source(here::here(file))
    flog.info(paste("Loaded UI module:", file))
  })
}
load_ui_modules()
# Fonction pour générer les dimensions
generate_dimension <- function(variable) {
  input_id <- paste0("select_", variable)
  column_name <- variable
  list(input_id = input_id, column_name = column_name)
}

# Générer les dimensions
dimensions <- lapply(variable_to_display, generate_dimension)

generate_target_variables <- function(variable) {
  target_name <- paste0("target_", variable)
  if (exists(target_name, envir = .GlobalEnv)) {
    return(base::get(target_name, envir = .GlobalEnv))
  } else {
    target_name <- paste0("target_", variable)
    target <- default_dataset%>%
      dplyr::select(!!sym(variable)) %>% 
      distinct()
    assign(target_name, target, envir = .GlobalEnv)
    return(target)
  }
}

# Générer targetVariables et targettes
targetVariables <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)

targetVariables2 <- lapply(targetVariables, as.data.frame)

targettes <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)
# Initialize color palettes with a fixed seed for reproducibility
palettes <- initialiserPalettes(targetVariables2, seed = 2643598)

# Define function to get target values based on category
getTarget <- function(category) {
  target <- targettes[[category]]
  return(target)
}

# Log the initialization of palettes
flog.info("Color palettes initialized.")

# if(exists("debug_mode") && debug_mode){
# default_dataset_preloaded <- readRDS(here::here("data/default_dataset_preloaded.rds"))
# } else {
#   default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
# }

# Adding resource path to display html -------------------------------------
addResourcePath("www", here::here("www"))

sql_query_init <- readRDS(here::here("tab_panels/sql_query_init.rds"))
flog.info("SQl query loaded")

map_init <- read_html(here::here("www/map_init.html"))
flog.info("Map init loaded")

# Load UI modules
source(here::here("R/load_ui_modules.R"))
load_ui_modules()

default_dataset_preloaded <- default_dataset


# Log that the UI and server files have been sourced successfully
flog.info("Global.R file loaded")
