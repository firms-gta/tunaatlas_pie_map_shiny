# Restore the renv environment, excluding shinyuieditor
# renv::restore(exclude = c("shinyuieditor"))

# Source install script
source(here::here('install.R'))

# Load functions from external sources
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i6_SpeciesMap.R")
source("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/R/TunaAtlas_i11_CatchesByCountry.R")

# Log the loading of libraries
flog.info("All libraries loaded successfully.")

# Load environment variables from file
try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))

# Create database connection pool
db_host <- Sys.getenv("DB_HOST")
db_port <- as.integer(Sys.getenv("DB_PORT"))
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER_READONLY")
db_password <- Sys.getenv("DB_PASSWORD")

pool <- dbPool(RPostgreSQL::PostgreSQL(),
               host = db_host,
               port = db_port,
               dbname = db_name,
               user = db_user,
               password = db_password)

# Log the successful creation of the connection pool
flog.info("Database connection pool to '%s' has been created successfully.", db_name)

# Initialize reactive values
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1)

# Query distinct values from the database for filters
target_dataset <- dbGetQuery(pool, "SELECT DISTINCT(dataset) FROM public.shinycatch ORDER BY dataset;")
target_species <- dbGetQuery(pool, "SELECT DISTINCT(species) FROM public.shinycatch ORDER BY species;")
target_year <- dbGetQuery(pool, "SELECT DISTINCT(year) FROM public.shinycatch ORDER BY year;")
target_flag <- dbGetQuery(pool, "SELECT DISTINCT(fishing_fleet) FROM public.shinycatch ORDER BY fishing_fleet;")
target_gridtype <- dbGetQuery(pool, "SELECT DISTINCT(gridtype) FROM public.shinycatch ORDER BY gridtype;")
target_gear_type <- dbGetQuery(pool, "SELECT DISTINCT(gear_type) FROM public.shinycatch ORDER BY gear_type;")
target_measurement_unit <- dbGetQuery(pool, "SELECT DISTINCT(measurement_unit) FROM public.shinycatch ORDER BY measurement_unit;")
target_fishing_mode <- dbGetQuery(pool, "SELECT DISTINCT(fishing_mode) FROM public.shinycatch ORDER BY fishing_mode;")

# Log the successful retrieval of filter options
flog.info("Filter options retrieved from database.")

# Set default values for filters
default_dataset <- ifelse('global_catch_firms_level0' %in% target_dataset$dataset, "global_catch_firms_level0", target_dataset[[1]][1])

# Log the default dataset selected
flog.info(paste("Default dataset selected:", default_dataset))

filters_combinations <- dbGetQuery(pool, "SELECT dataset, gear_type, gridtype, species, year, fishing_fleet, fishing_mode, measurement_unit FROM public.shinycatch GROUP BY dataset, gear_type, gridtype, species, year, fishing_fleet, fishing_mode, measurement_unit;")

# Set default filter values based on combinations
default_gridtype <- filters_combinations %>%
  dplyr::filter(dataset == default_dataset) %>%
  head(1) %>%
  pull(gridtype)

default_measurement_unit <- "t"

default_species <- filters_combinations %>%
  dplyr::filter(dataset == default_dataset) %>%
  dplyr::filter(gridtype == default_gridtype) %>%
  dplyr::filter(measurement_unit == default_measurement_unit) %>%
  head(5) %>%
  pull(species)

filtered_combinations_default <- filters_combinations %>%
  dplyr::filter(dataset == default_dataset) %>%
  dplyr::filter(measurement_unit == default_measurement_unit) %>%
  dplyr::filter(gridtype == default_gridtype)

default_flag <- unique(filtered_combinations_default$fishing_fleet)
default_gear_type <- unique(filtered_combinations_default$gear_type)
default_fishing_mode <- unique(filtered_combinations_default$fishing_mode)

# Log default filter values
flog.info(paste("Default filter values set: Gridtype:", default_gridtype, ", Measurement Unit:", default_measurement_unit))

variable_to_display <- c("species", "fishing_fleet", "measurement_value", "gear_type", "fishing_mode")

# Define analysis options
analysis_options <- list(
  list(title = "Species Analysis", id = "species_analysis"),
  list(title = "Fishing Fleet Analysis", id = "fleet_analysis"),
  list(title = "Gear Type Analysis", id = "gear_analysis"),
  list(title = "Fishing Mode Analysis", id = "fishing_mode_analysis")
)

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
    'modules/categoryGlobalPieChart.R',
    'modules/pieMapTimeSeriesUI.R',
    'R/get_html_title.R',
    'R/getPalette.R',
    'R/palette_settings.R'
  )
  lapply(ui_files, function(file) {
    source(here::here(file))
    flog.info(paste("Loaded UI module:", file))
  })
}
load_ui_modules()

# Prepare target variables list
targetVariables <- list(
  species = target_species,
  fishing_fleet = target_flag,
  gear_type = target_gear_type,
  fishing_mode = target_fishing_mode
)

# Initialize color palettes with a fixed seed for reproducibility
palettes <- initialiserPalettes(targetVariables, seed = 2643598)


targettes <- list(
  species = target_species,        
  fishing_fleet = target_flag, 
  gear_type = target_gear_type, fishing_mode = target_fishing_mode
)
# Define function to get target values based on category
getTarget <- function(category) {
  target <- targettes[[category]]
  return(target)
}


# Log the initialization of palettes
flog.info("Color palettes initialized.")


createSQLQuery <- function(dataset_name = default_dataset,
                           species_name = default_species,
                           fishing_fleet_name = default_flag,
                           gear_type_name = default_gear_type,
                           selected_years = target_year$year,
                           measurement_unit_name = default_measurement_unit,
                           fishing_mode_name = default_fishing_mode,
                           wkt = global_wkt,
                           con = pool,
                           limit = NULL) {
  
  limit_clause <- if (!is.null(limit)) glue::glue_sql("LIMIT {limit}", .con = con) else SQL("")
  
  query <- glue::glue_sql(
    "SELECT geom_id, geom, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode,
    ST_asText(geom) AS geom_wkt, year FROM public.shinycatch
    WHERE dataset IN ({dataset_name})
    AND ST_Within(geom, ST_GeomFromText(({wkt*}), 4326))
    AND fishing_fleet IN ({fishing_fleet_name*})
    AND species IN ({species_name*})
    AND gear_type IN ({gear_type_name*})
    AND year IN ({selected_years*})
    AND measurement_unit IN ({measurement_unit_name*})
    AND fishing_mode IN ({fishing_mode_name*})
    GROUP BY species, fishing_fleet, geom_id, geom_wkt, geom, year, gear_type, fishing_mode, measurement_unit
    ORDER BY species, fishing_fleet DESC {limit_clause}",
    .con = pool
  )
  
  flog.info("SQL query created successfully.")
  return(query)
}

# Recreate overview details if not existing -------------------------------

source(here::here("R/initialize_data_and_plots.R"))


# Adding resource path to display html -------------------------------------

addResourcePath("www", here::here("www"))
# Source UI and server files
source(here::here("ui.R"))
source(here::here("server.R"))

# Log that the UI and server files have been sourced successfully
flog.info("UI and server files sourced successfully.")

