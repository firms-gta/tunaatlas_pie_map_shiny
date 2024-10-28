# Creating default dataset on this basis and if exists already loading it
# source(here::here('install.R'))
flog.info("Sourced create or load defautl dataset")
if(!file.exists("data/default_dataset") & !exists("default_dataset")){
  
  flog.info("Loading data ")
  # Read the DOI CSV file
  
  DOI <- read_csv('data/DOI.csv')
  source(here::here("update_data.R"))
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
  
  object <- tools::file_path_sans_ext(DOI$Filename[1])
  source(here::here("download_GTA_data.R"))
  # Load the shapefile
  shapefile_path <- here::here("data/cl_areal_grid.csv")
  flog.info("Loading shapefile from %s", shapefile_path)
  shapefile.fix <- read.csv(shapefile_path)
  
  # Convert to sf object
  flog.info("Converting shapefile to sf object")
  shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt")
  if (is.na(st_crs(shapefile.fix))) {
    flog.warn("No CRS found, setting to WGS84")
    st_crs(shapefile.fix) <- 4326  # Set to WGS84
  }
  
  # Set tmap options
  flog.info("Setting tmap options")
  tmap_options(check.and.fix = TRUE)
  
  flog.info("Filtering and renaming shapefile data")
  shapefile.fix <- shapefile.fix[sf::st_is_valid(shapefile.fix),]
  shapefile.fix <- shapefile.fix %>% 
    dplyr::rename(cwp_code = CWP_CODE, gridtype = GRIDTYPE, geom = geom_wkt) %>% 
    dplyr::select(cwp_code, gridtype, geom) %>%
    dplyr::mutate(cwp_code = as.character(cwp_code))
  
  flog.info("Converting shapefile to data.table")
  shapefile.fix <- as.data.table(shapefile.fix)
  
  flog.info("Loading species data")
  species <- read_csv(here::here("data/cl_species.csv")) %>% 
    dplyr::select(code, label, taxa_order) %>% 
    dplyr::rename(code_species = code, species_name = label, species_group = taxa_order) %>% 
    distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv
  flog.info("Loaded species and species_group data")
  
  flog.info("Loading cl_cwp_gear_level2 data")
  cl_cwp_gear_level2 <- read_csv(here::here("data/cl_cwp_gear_level2.csv")) %>% 
    dplyr::select(Code = code, Gear = label) %>% 
    dplyr::distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv
  flog.info("Loaded cl_cwp_gear_level2 data")
  
  flog.info(sprintf("Time %s:", Sys.time()))
  
  flog.info("Loading default dataset")
  default_dataset <- base::get(object)
  
  # Convert species and cl_cwp_gear_level2 to data.table
  flog.info("Converting species and cl_cwp_gear_level2 to data.table")
  species <- as.data.table(species)
  cl_cwp_gear_level2 <- as.data.table(cl_cwp_gear_level2)
  
  # Convert default_dataset to data.table
  flog.info("Converting default_dataset to data.table to make all the operations from 30 seconds to 15 seconds")
  setDT(default_dataset)
  
  # Perform operations on default_dataset with data.table
  flog.info("Filtering and selecting columns in default_dataset")
  default_dataset <- default_dataset[
    !is.na(measurement_value), 
    .(year = year(time_start), 
      month = month(time_start), 
      measurement_value, 
      measurement_unit, 
      species, 
      gear_type, 
      source_authority,
      measurement,
      measurement_type,
      geographic_identifier, 
      fishing_mode, 
      fishing_fleet)
  ]
  
  flog.info("Merging default_dataset with species data")
  default_dataset <- merge(default_dataset, species, by.x = "species", by.y = "code_species", all.x = TRUE)
  
  flog.info("Merging default_dataset with cl_cwp_gear_level2 data")
  default_dataset <- merge(default_dataset, cl_cwp_gear_level2, by.x = "gear_type", by.y = "Code", all.x = TRUE)
  default_dataset[, gear_type := as.character(gear_type)]
  default_dataset[, geographic_identifier := as.character(geographic_identifier)]
  
  flog.info("Merging default_dataset with shapefile.fix to add geometry")
  default_dataset <- merge(default_dataset, shapefile.fix, by.x = "geographic_identifier", by.y = "cwp_code", all.x = TRUE)
  default_dataset <- as.data.frame(default_dataset) %>% dplyr::rename(geom_wkt = geom)
  
  flog.info(sprintf("Time %s:", Sys.time()))
  flog.info(sprintf("Colnames %s:", paste0(colnames(default_dataset))))
  
  # source(here::here("R/initialize_data_and_plots.R"))
  default_dataset <- as.data.frame(default_dataset)
  qs::qsave(default_dataset, "data/default_dataset")
  
  # Log the initialization of palettes
  flog.info("Color palettes initialized.")
  
} else if(!exists("default_dataset") & file.exists("data/default_dataset")){
  flog.info("reading the data from qs file")
  default_dataset <- qs::qread("data/default_dataset")
}