# Load required packages
lapply(c("here", "futile.logger", "readr", "tools", "sf", "dplyr", "data.table", "qs"), require, character.only = TRUE)
flog.info("Sourced create or load default dataset")

# Paths for processed files
default_dataset_path <- here::here("data/default_dataset.qs")

DOI <- read_csv("DOI.csv")

if(!file.exists(here::here("data/default_dataset.qs")) & !exists("default_dataset")){
  
  flog.info("Loading data ")
  # Read the DOI CSV file
  i <- 1
  record_id <- sub(".*zenodo\\.([0-9]+)$", "\\1", DOI$DOI[i])
  
  filename <- DOI$Filename[i]
  dataset <- tools::file_path_sans_ext(filename)
  renamed <- file.path("data", paste0(dataset, "_", record_id, "_updated.qs"))
  default_dataset <- qs::qread(renamed)
  flog.info("Saving default dataset to qs file")
  qs::qsave(default_dataset, here::here("data/default_dataset.qs"))
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
