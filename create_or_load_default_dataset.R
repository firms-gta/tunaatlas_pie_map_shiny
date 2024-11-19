# Load required packages
lapply(c("here", "futile.logger", "readr", "tools", "sf", "tmap", "dplyr", "data.table", "qs"), require, character.only = TRUE)
flog.info("Sourced create or load default dataset")

# Paths for processed files
cl_areal_grid_path <- here::here("data/cl_areal_grid.qs")
cl_species_path <- here::here("data/cl_species.qs")
cl_cwp_gear_path <- here::here("data/cl_cwp_gear_level2.qs")
default_dataset_path <- here::here("data/default_dataset.qs")

# Load or process cl_areal_grid
if (file.exists(here::here(cl_areal_grid_path))) {
  flog.info("Loading processed cl_areal_grid from .qs")
  shapefile.fix <- qs::qread(here::here(cl_areal_grid_path))
} else {
  flog.info("Processing cl_areal_grid and saving as .qs")
  shapefile_path <- here::here("data/cl_areal_grid.csv")
  shapefile.fix <- read.csv(shapefile_path)
  shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt")
  if (is.na(st_crs(shapefile.fix))) {
    flog.warn("No CRS found, setting to WGS84")
    st_crs(shapefile.fix) <- 4326
  }
  shapefile.fix <- shapefile.fix[sf::st_is_valid(shapefile.fix),]
  shapefile.fix <- shapefile.fix %>% 
    rename(geographic_identifier = CWP_CODE, gridtype = GRIDTYPE) %>% 
    select(geographic_identifier, gridtype, geom_wkt) %>%
    mutate(geographic_identifier = as.character(geographic_identifier))
  shapefile.fix <- st_as_sf(shapefile.fix)
  qs::qsave(shapefile.fix, cl_areal_grid_path)
  shapefile.fix$geom_wkt <- NULL
  qs::qsave(shapefile.fix, "data/gridtype.qs")
}

# Load or process cl_species
if (file.exists(here::here(cl_species_path))) {
  flog.info("Loading processed cl_species from .qs")
  species <- qs::qread(here::here(cl_species_path))
} else {
  flog.info("Processing cl_species and saving as .qs")
  species <- read_csv(here::here("data/cl_species.csv")) %>% 
    select(code, label, taxa_order) %>% 
    rename(code_species = code, species_name = label, species_group = taxa_order) %>% 
    distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv
  species <- as.data.table(species)
  qs::qsave(species, cl_species_path)
}

# Load or process cl_cwp_gear_level2
if (file.exists(here::here(cl_cwp_gear_path))) {
  flog.info("Loading processed cl_cwp_gear_level2 from .qs")
  cl_cwp_gear_level2 <- qs::qread(here::here(cl_cwp_gear_path))
} else {
  flog.info("Processing cl_cwp_gear_level2 and saving as .qs")
  cl_cwp_gear_level2 <- read_csv(here::here("data/cl_cwp_gear_level2.csv")) %>% 
    select(Code = code, Gear = label) %>% 
    distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv
  cl_cwp_gear_level2 <- as.data.table(cl_cwp_gear_level2)
  qs::qsave(cl_cwp_gear_level2, here::here(cl_cwp_gear_path))
}

if(!file.exists(here::here("data/default_dataset.qs")) & !exists("default_dataset")){
  
  flog.info("Loading data ")
  # Read the DOI CSV file
  
  DOI <- read_csv('data/DOI.csv')
  source(here::here("update_data.R"))
  load_data <- function(DOI) {
    # loaded_data <- list()
    
    for (filename in DOI$Filename) {
      flog.info("Loading dataset: %s", filename)
      
      # Define file paths
      base_filename <- tools::file_path_sans_ext(filename)
      qs_file_path <- file.path('data', paste0(base_filename, '.qs'))
      csv_file_path <- file.path('data', paste0(base_filename, '.csv'))
      rds_file_path <- file.path('data', paste0(base_filename, '.rds'))
      
      # Check if .qs file exists
      if (file.exists(here::here(qs_file_path))) {
        flog.info("Load from qs")
        data <- qs::qread(here::here(qs_file_path))
        flog.info("Loaded %s from .qs", filename)
        
      } else {
        # If .qs does not exist, try to load from CSV or RDS
        if (file.exists(here::here(csv_file_path))) {
          # Load from CSV with specific column type
          data <- read_csv(here::here(csv_file_path), col_types = cols(gear_type = col_character()))
          flog.info("Loaded %s from CSV", filename)
          
        } else if (file.exists(here::here(rds_file_path))) {
          # Load from RDS
          data <- readRDS(here::here(rds_file_path))
          flog.info("Loaded %s from RDS", filename)
          
          # Ensure gear_type is character after reading from RDS
          if ("gear_type" %in% names(data)) {
            data$gear_type <- as.character(data$gear_type)
          }
        } else {
          # File not found
          warning(paste('File not found:', csv_file_path, 'or', rds_file_path))
          next
        }
        
        # Save the loaded data to .qs for faster future access
        qs::qsave(data, qs_file_path)
        flog.info("Saved %s as .qs", filename)
        
        # Add to the loaded_data list and assign to global environment
        loaded_data[[base_filename]] <- data
      }
      
      # Assign the loaded data to the global environment
      assign(base_filename, as.data.frame(loaded_data[[base_filename]]), envir = .GlobalEnv)
    }
  }
  
  
  load_data(DOI)
  
  object <- tools::file_path_sans_ext(DOI$Filename[1])
  source(here::here("download_GTA_data.R"))
  # Load the shapefile
  
  flog.info("Loading species data")
  species <- qs::qread("data/cl_species.qs")
  flog.info("Loaded species and species_group data")
  
  flog.info("Loading cl_cwp_gear_level2 data")
  cl_cwp_gear_level2 <- qs::qread("data/cl_cwp_gear_level2.qs")
  flog.info("Loaded cl_cwp_gear_level2 data")
  
  flog.info(sprintf("Time %s:", Sys.time()))
  
  flog.info("Loading default dataset")
  default_dataset <- base::get(object)
  
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
  
  gridtype <- qs::qread("data/gridtype.qs")
  # flog.info("Merging default_dataset with shapefile.fix to add geometry")
  default_dataset <- merge(default_dataset, gridtype, by.x = "geographic_identifier", by.y = "geographic_identifier", all.x = TRUE)
  default_dataset <- as.data.frame(default_dataset)
  
  flog.info(sprintf("Time %s:", Sys.time()))
  flog.info(sprintf("Colnames %s:", paste0(colnames(default_dataset))))
  
  # geom <- default_dataset %>% 
  #   dplyr::select(geom_wkt, geographic_identifier) %>% 
  #   dplyr::distinct()
  # source(here::here("R/initialize_data_and_plots.R"))
  # default_dataset$geom_wkt <- NULL
  qs::qsave(default_dataset, "data/default_dataset.qs")
  # qs::qsave(geom, "data/geom.qs")
  
} else if(!exists("default_dataset") & file.exists("data/default_dataset.qs")){
  flog.info("reading the data from qs file")
  default_dataset <- qs::qread("data/default_dataset.qs")
  flog.info("Data read")
  flog.info(paste0("colnames of default dataset:", colnames(default_dataset)))
  flog.info(paste0("class of default dataset", class(default_dataset)))
                      
  # geom <- qs::qread("data/geom.qs")
  # default_dataset_shape <- default_dataset %>% dplyr::inner_join(shapefile.fix, by = c("geographic_identifier" = "cwp_code"))
  
}
