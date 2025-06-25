# Load required packages
lapply(c("here", "futile.logger", "readr", "tools", "sf", "tmap", "dplyr", "data.table", "qs", "CWP.dataset"), require, character.only = TRUE)
flog.info("Sourced create or load default dataset")

# Paths for processed files
default_dataset_path <- here::here("data/default_dataset.qs")
cl_areal_grid_path <- here::here("data/cl_areal_grid.qs")


if(!file.exists(cl_areal_grid_path)){
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  if (!file.exists(cwp_grid_file)) {
    stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
  }
  shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 geom     = geom_wkt) %>% 
      dplyr::select(geom, geographic_identifier = code, gridtype = GRIDTYPE) %>% dplyr::distinct()
  
  qs::qsave(shapefile.fix, cl_areal_grid_path)
} else {
  shapefile.fix <- qs::qread(cl_areal_grid_path)
}

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

