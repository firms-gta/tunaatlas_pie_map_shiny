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
load_data <- function() {
  loaded_data <- list()
  
  for (filename in DOI$Filename) {
    if(!exists(tools::file_path_sans_ext(filename))){
      
      flog.info("Loading dataset: %s", filename)
      
      base_filename <- tools::file_path_sans_ext(filename) # Remove any existing extension
      csv_file_path <- file.path('data', paste0(base_filename, '.csv'))
      rds_file_path <- file.path('data', paste0(base_filename, '.rds'))
      
      if (file.exists(csv_file_path)) {
        assign(base_filename, fread(csv_file_path), envir = .GlobalEnv)
        loaded_data[[base_filename]] <- read_csv(csv_file_path)
      } else if (file.exists(rds_file_path)) {
        assign(base_filename, readRDS(rds_file_path), envir = .GlobalEnv)
        loaded_data[[base_filename]] <- readRDS(rds_file_path)
      } else {
        warning(paste('File not found:', csv_file_path, 'or', rds_file_path))
      }
    } else {
      flog.info("Dataset %s already existing, no need to load it", tools::file_path_sans_ext(filename))
      
    }
  }
  
  # return(loaded_data)
}

load_data()

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
# target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")
# target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
# target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
# target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.shinycatch ORDER BY fishing_fleet;")
# target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.shinycatch ORDER BY gridtype;")
# target_gear_type <- dbGetQuery(pool, "SELECT DISTINCT(gear_type) FROM public.shinycatch ORDER BY gear_type;")
# target_measurement_unit <- dbGetQuery(pool, "SELECT DISTINCT(measurement_unit) FROM public.shinycatch ORDER BY measurement_unit;")
# target_fishing_mode <- dbGetQuery(pool, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

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
default_dataset <- ifelse('global_catch_5deg_1m_firms_level1' %in% target_dataset$dataset, "global_catch_5deg_1m_firms_level1", target_dataset[[1]][1])

# Log the default dataset selected
flog.info(paste("Default dataset selected:", default_dataset))

filters_combinations_query <- glue::glue_sql("SELECT dataset, measurement_unit, gridtype FROM public.shinycatch GROUP BY dataset, measurement_unit, gridtype;",
                                       .con = pool)

filters_combinations <- DBI::dbGetQuery(pool, filters_combinations_query)

# # Set default filter values based on combinations
default_gridtype <- filters_combinations %>%
  dplyr::filter(dataset == default_dataset) %>%
  head(1) %>%
  pull(gridtype)

default_measurement_unit <- "t"
# 
# default_species <- filters_combinations %>%
#   dplyr::filter(dataset == default_dataset) %>%
#   dplyr::filter(gridtype == default_gridtype) %>%
#   dplyr::filter(measurement_unit == default_measurement_unit) %>%
#   head(5) %>%
#   pull(species)
# 
# filtered_combinations_default <- filters_combinations %>%
#   dplyr::filter(dataset == default_dataset) %>%
#   dplyr::filter(measurement_unit == default_measurement_unit) %>%
#   dplyr::filter(gridtype == default_gridtype)
# 
# default_flag <- unique(filtered_combinations_default$fishing_fleet)
# default_gear_type <- unique(filtered_combinations_default$gear_type)
# default_fishing_mode <- unique(filtered_combinations_default$fishing_mode)

# Log default filter values
# flog.info(paste("Default filter values set: Gridtype:", default_gridtype, ", Measurement Unit:", default_measurement_unit))

# Get all column names
global_catch_tunaatlasird_level2 <- fread("data/global_catch_tunaatlasird_level2.csv")

if(value%in%colnames(global_catch_tunaatlasird_level2)){
global_catch_tunaatlasird_level2 <- global_catch_tunaatlasird_level2 %>% dplyr::rename(#measurement_value = value,
                                                                                       fishing_fleet = flag,
                                                                                       gear_type = gear,
                                                                                       fishing_mode = schooltype,
                                                                                      measurement_unit = catchunit) %>%
  dplyr::select(species, measurement_value, fishing_fleet, year, month, gear_type, fishing_mode, geom, geom_wkt, measurement_unit)

global_catch_tunaatlasird_level2$gridtype <- "5deg_x_5deg"
}

non_numeric_columns <- names(global_catch_tunaatlasird_level2)[sapply(global_catch_tunaatlasird_level2, is.character)]

# Create target_* variables for each non-numeric column
for (col in non_numeric_columns) {
  assign(paste0("target_", col), unique(global_catch_tunaatlasird_level2[[col]]))
}
variable_to_display <- non_numeric_columns
variable_to_display <- c("gear","fishing_fleet", "species", "fishing_mode"# , "species_label", "flag_label", "measurement_value" # "gear_type",
                    #     "gear_label","fishing_mode", "schooltype_label", "catchtype_label", "species_group_labels", "gear_group_label"
                    )

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
    "modules/dataset_choice.R"
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
    warning(paste("Target variable", target_name, "does not exist"))
    return(NULL)
  }
}

# Function to generate data frame for each column
generate_data_frame <- function(data) {
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

# call_modules <- function(variable_to_display, data_without_geom, final_filtered_data, centroid, submitTrigger) {
#   lapply(variable_to_display, function(variable) {
#     categoryGlobalPieChartServer(paste0(variable, "_chart"), variable, data_without_geom)
#     pieMapTimeSeriesServer(paste0(variable, "_module"), variable, final_filtered_data, centroid, submitTrigger)
#     TimeSeriesbyDimensionServer(paste0(variable, "_timeseries"), variable, data_without_geom)
#   })
# } marche pas bien

# Log that the UI and server files have been sourced successfully
flog.info("Global.R file loaded")

