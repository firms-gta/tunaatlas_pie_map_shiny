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
library(data.table)
library(readr)
library(futile.logger)

library(data.table)
library(readr)
library(futile.logger)

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

# Initialize reactive values
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1)

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

load_target_data <- function(file_path) {
  target_data <- readRDS(file_path)
  list2env(target_data, .GlobalEnv)
}

# Call the function to load data
load_target_data("data/target.rds")

# Log the successful retrieval of filter options
flog.info("Filter options retrieved from database.")

# Set default values for filters
# default_dataset <- ifelse('global_catch_5deg_1m_firms_level1' %in% target$target_dataset$dataset, "global_catch_5deg_1m_firms_level1", target_dataset[[1]][1])
default_dataset_DB <- ifelse('global_catch_5deg_1m_firms_level1' %in% target_dataset$dataset, "global_catch_5deg_1m_firms_level1", target_dataset[[1]][1])

# Log the default dataset selected
flog.info(paste("Default dataset DB selected:", default_dataset_DB))

filters_combinations_query <- glue::glue_sql("SELECT dataset, measurement_unit, gridtype FROM public.shinycatch GROUP BY dataset, measurement_unit, gridtype;",
                                       .con = pool)

filters_combinations <- DBI::dbGetQuery(pool, filters_combinations_query)

# # Set default filter values based on combinations
default_gridtype <- filters_combinations %>%
  dplyr::filter(dataset == default_dataset_DB) %>%
  head(1) %>%
  pull(gridtype)

default_measurement_unit <- "t"

shapefile.fix <- st_read(pool,query = "SELECT * from area.cwp_grid") 

species_group <- st_read(pool, query = "SELECT taxa_order, code FROM species.species_asfis") %>%
  dplyr::select(species_group = taxa_order, species = code)%>% dplyr::distinct()
flog.info("Loaded species_group data")

cl_cwp_gear_level2 <- st_read(pool, query = "SELECT * FROM gear_type.isscfg_revision_1") %>%
  dplyr::select(Code = code, Gear = label)%>% dplyr::distinct()
flog.info("Loaded cl_cwp_gear_level2 data")

object <- tools::file_path_sans_ext(DOI$Filename[1])
default_dataset <- base::get(object)
default_dataset <- default_dataset%>% dplyr::mutate(gear_type = as.character(gear_type)) %>% 
  dplyr::filter(measurement_unit == "t") %>% dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% 
  full_join(shapefile.fix %>% dplyr::select(cwp_code, gridtype) , by = c("geographic_identifier" = "cwp_code")) %>% 
  dplyr::rename(geom_wkt = geom) %>% 
  dplyr::mutate(year = lubridate::year(time_start)) %>% 
  dplyr::mutate(month = lubridate::month(time_start)) %>% 
  dplyr::filter(!is.na(measurement_value))%>%
  dplyr::select(-c(time_start, time_end,  quarter)) %>% 
  dplyr::left_join(species_group, by = c("species")) %>% 
  dplyr::left_join(cl_cwp_gear_level2, by = c("gear_type" = "Code"))


default_dataset <- as.data.frame(default_dataset)

non_numeric_columns <- names(default_dataset)[sapply(default_dataset, is.character)]

# Create target_* variables for each non-numeric column
for (col in non_numeric_columns) {
  assign(paste0("target_", col), unique(default_dataset[[col]]))
}

variable_to_display <- non_numeric_columns
variable_to_display <- c("fishing_fleet", "species", "fishing_mode",
                         "source_authority", "measurement", "measurement_type", 
                         "gridtype", "species_group", 
                         "gear_type",
                         "Gear" )

# variable_to_display1 <- c("gear_type","fishing_fleet", "species", "fishing_mode"#, "source_authority",
#                          #"measurement_unit", "measurement","fishing_mode" , "measurement_type"
#                     )

# Fonction pour générer les titres et IDs d'analyse
generate_analysis_option <- function(variable) {
  title <- paste0(toupper(substring(variable, 1, 1)), substring(variable, 2), " Analysis")
  id <- paste0(variable, "_analysis")
  list(title = title, id = id)
}

# Générer les options d'analyse
analysis_options <- lapply(variable_to_display, generate_analysis_option)

# Load UI modules
load_ui_modules <- function() {
  ui_files <- c(
    'tab_panels/geographic_catches_ui.R',
    'tab_panels/main_panel_ui.R',
    'tab_panels/geographic_catches_by_variable_ui.R',
    'tab_panels/ggplot_indicator_11_ui.R',
    'tab_panels/additional_info_ui.R',
    'tab_panels/filterUI.R',
    'tab_panels/data_explorer_combined_ui.R',
    'tab_panels/total_catch_plot.R',
    'tab_panels/sidebar_ui.R',
    'tab_panels/mapCatchesmodules.R',
    'tab_panels/create_logo_panel.R',
    'tab_panels/dataset_choice.R',
    'tab_panels/sqlqueriesui.R',
    'tab_panels/data_explorer_i11_ui.R',
    'tab_panels/more_about.R',
    'rmd/rendering_rmd_files_to_html.R',
    'modules/generateRmdNavMenu.R',
    'modules/TimeSeriesbyDimension.R',
    'modules/categoryGlobalPieChart.R',
    'modules/pieMapTimeSeriesUI.R',
    'modules/plotTotalCatches.R',
    'R/get_html_title.R',
    'R/getPalette.R',
    'R/palette_settings.R', 
    "modules/dataset_choice.R", 
    "R/data_loading.R", 
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

# #dimensions 
# dimensions <- lapply(variable_to_display1, generate_dimension)

generate_target_variables <- function(variable) {
  target_name <- paste0("target_", variable)
  if (exists(target_name, envir = .GlobalEnv)) {
    return(base::get(target_name, envir = .GlobalEnv))
  } else {
    target_name <- paste0("target_", variable)
    target <- default_dataset%>%
      dplyr::select(!!sym(variable)) %>% 
      distinct()
    return(base::set(target_name, value = target))
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

# createSQLQuery <- function(dataset_name = default_dataset,
#                            species_name = default_species,
#                            fishing_fleet_name = default_flag,
#                            gear_type_name = default_gear_type,
#                            gridtype_name = default_gridtype,
#                            selected_years = target_year$year,
#                            measurement_unit_name = default_measurement_unit,
#                            fishing_mode_name = default_fishing_mode,
#                            wkt = global_wkt,
#                            con = pool,
#                            limit = NULL) {
#   
#   limit_clause <- if (!is.null(limit)) glue::glue_sql("LIMIT {limit}", .con = con) else SQL("")
#   
#   query <- glue::glue_sql(
#     "SELECT gridtype, geom_id, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, geom,
#     ST_asText(geom) AS geom_wkt, year, month FROM public.shinycatch
#     WHERE dataset IN ({dataset_name})
#     AND ST_Within(geom, ST_GeomFromText(({wkt*}), 4326))
#     AND fishing_fleet IN ({fishing_fleet_name*})
#     AND species IN ({species_name*})
#     AND gear_type IN ({gear_type_name*})
#     AND gridtype IN ({gridtype_name*})
#     AND year IN ({selected_years*})
#     AND fishing_mode IN ({fishing_mode_name*})
#     AND measurement_unit IN ({measurement_unit_name*})
#     GROUP BY gridtype, species, fishing_fleet, geom_id, geom_wkt, geom, year, month, gear_type, fishing_mode, measurement_unit
#     ORDER BY species, fishing_fleet DESC {limit_clause}",
#     .con = pool
#   )
#   
#   flog.info("SQL query created successfully.")
#   return(query)
# }


# if(exists("debug_mode") && debug_mode){
# default_dataset_preloaded <- readRDS(here::here("data/default_dataset_preloaded.rds"))
# } else {
#   default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
# }

# Recreate overview details if not existing -------------------------------

# Adding resource path to display html -------------------------------------

# addResourcePath("www", here::here("www"))
# Source UI and server files
# source(here::here("ui.R"))
# source(here::here("server.R"))
# source(here::here("server.R"))
addResourcePath("www", here::here("www"))

sql_query_init <- readRDS(here::here("tab_panels/sql_query_init.rds"))
flog.info("SQl query loaded")

map_init <- read_html(here::here("www/map_init.html"))
flog.info("Map init loaded")

default_dataset_preloaded <- default_dataset


# Log that the UI and server files have been sourced successfully
flog.info("Global.R file loaded")
