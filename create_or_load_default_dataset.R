# Load required packages
lapply(c("here", "futile.logger", "readr", "tools", "sf", "dplyr", "data.table", "qs"), require, character.only = TRUE)
flog.info("Sourced create or load default dataset")

variable <- c("source_authority",
              "species",
              # "species_label",
              # "fishing_fleet_label",
              "fishing_fleet",
              "FLEET_CODE",
              "GEAR_CODE",
              "SPECIES_CODE",
              "SCHOOL_TYPE_CODE",
              "CLASS_LOW",
              "CLASS_HIGH",
              "RAISE_CODE",
              "REPORTING_QUALITY",
              # "gear_type_label",
              "gear_type",
              # "species_name",
              "fishing_mode",
              # "fishing_mode_label", 
              "measurement_unit",
              # "measurement_unit_label",
              "gridtype",
              # "measurement",
              "measurement_type",
              # "measurement_type_label",
              "species_group", 
              # "species", 
              "issue", 
              "species_group"
)

# Paths for processed files
default_dataset_path <- here::here("data/default_dataset.qs")

DOI <- readr::read_csv("DOI.csv")
if(!file.exists("data/data.qs")){
if(!file.exists(here::here("data/default_dataset.qs")) & !exists("default_dataset")){
  
  flog.info("Loading data ")
  # Read the DOI CSV file
  i <- 1
  record_id <- sub(".*zenodo\\.([0-9]+)$", "\\1", DOI$DOI[i])
  
  filename <- DOI$Filename[i]
  dataset <- tools::file_path_sans_ext(filename)
  renamed <- file.path("data", paste0(dataset, "_", record_id, "_updated.qs"))
  if(file.exists(here::here(renamed))){
  default_dataset <- qs::qread(here::here(renamed))
  flog.info("Saving default dataset to qs file")
  qs::qsave(default_dataset, here::here("data/default_dataset.qs"))
  }
  # updated <- file.path("data", paste0(dataset, "_updated.qs"))
  
 } else if(!exists("default_dataset") & file.exists("data/default_dataset.qs")){
  flog.info("reading the data from qs file")
  default_dataset <- qs::qread("data/default_dataset.qs")
  flog.info("Data read")
  flog.info(paste0("colnames of default dataset:", colnames(default_dataset)))
  flog.info(paste0("class of default dataset", class(default_dataset)))
  
  # geom <- qs::qread("data/geom.qs")
  # default_dataset_shape <- default_dataset %>% dplyr::inner_join(shapefile.fix, by = c("geographic_identifier" = "cwp_code"))
  
 }
  source(here::here("R/data_loading.R"))
  source(here::here("global/generate_dimensions_palettes.R"))
  data <- load_initial_data(default_dataset)
  res <- generate_dimensions_palettes(
    df = data$data_for_filters,
    variable = variable,           # ton vecteur global des variables possibles
    seed = 2643598
  )
  data$palettes  <- res$palettes
  data$targettes <- res$targettes
  data$variable_to_display <- res$variables
  qs::qsave(data,"data/data.qs")
} else if (is.function(data)){
  flog.info("reading data.qs")
  data <- qs::qread("data/data.qs")
  flog.info("data.qs read")
}
