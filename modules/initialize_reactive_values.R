# Initialize reactive values
flog.info("Initializing reactive values")
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1)
# Load environment variables from file
if (file.exists("connection_tunaatlas_inv.txt")) {
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
    flog.info("Database connection pool to '%s' has been created successfully.", db_name)
    
  }, error = function(e) {
    flog.error("Failed to create database connection pool: %s", e$message)
  })
  
  if (exists("pool") && pool::dbIsValid(pool)) {
    flog.info("DB pool exists and is valid")
    
    # load_target_data <- function(file_path) {
    #   target_data <- readRDS(file_path)
    #   list2env(target_data, .GlobalEnv)
    # }
    #
    # # Call the function to load data
    # load_target_data("data/target.rds")
    # Query distinct values from the database for filters
    
    filters_combinations_query <- glue::glue_sql("SELECT dataset, measurement_unit, gridtype FROM public.shinycatch GROUP BY dataset, measurement_unit, gridtype;",
                                                 .con = pool)
    
    flog.info("Executing query to get filters_combinations")
    filters_combinations <- DBI::dbGetQuery(pool, filters_combinations_query)
    
    flog.info("Setting default filter values based on combinations")
    default_dataset_DB <- ifelse('global_catch_5deg_1m_firms_level1' %in% filters_combinations$dataset, "global_catch_5deg_1m_firms_level1", filters_combinations[[1]][1])
    
    default_gridtype <- filters_combinations %>%
      dplyr::filter(dataset == default_dataset_DB) %>%
      head(1) %>%
      pull(gridtype)
    
    default_measurement_unit <- "t"
    
    # query <- sprintf("SELECT gridtype, codesource_area, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, year, month FROM public.shinycatch
    # WHERE dataset = '%s'
    # GROUP BY gridtype, species, fishing_fleet, codesource_area, year, month, gear_type, fishing_mode, measurement_unit", default_dataset_DB)
    # 
    # default_dataset <- DBI::dbGetQuery(pool, query) %>% dplyr::rename(geographic_identifier = codesource_area)
    
    
  } else {
    filters_combinations <- as.data.frame(NULL)
    default_gridtype <- NULL
    default_measurement_unit <- NULL
  }
} else {
  filters_combinations <- as.data.frame(NULL)
  default_gridtype <- NULL
  default_measurement_unit <- NULL
}

# shapefile.fix <- st_read(pool,query = "SELECT * from area.cwp_grid") 
# shapefile.fix <- st_read(pool,query = "SELECT * from area.cwp_grid_erased") #https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip
# flog.info("Loaded shapefile.fix data from DB")
# 
# species_group <- st_read(pool, query = "SELECT taxa_order, code FROM species.species_asfis") %>% #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv
#   dplyr::select(species_group = taxa_order, species = code)%>% dplyr::distinct()
# flog.info("Loaded species_group data from DB")
# 
# cl_cwp_gear_level2 <- st_read(pool, query = "SELECT * FROM gear_type.isscfg_revision_1") %>% #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv
#   dplyr::select(Code = code, Gear = label)%>% dplyr::distinct()
# flog.info("Loaded cl_cwp_gear_level2 data from DB")