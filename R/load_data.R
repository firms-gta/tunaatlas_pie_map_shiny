#' Load and Cache Data with Enrichment and Tracking
#'
#' This function loads datasets from various file formats (e.g., .csv, .rds)
#' based on the filenames provided in the `DOI` input. It enriches data
#' (via `enrich_data()`), adds time-based columns, and caches the result
#' in an "_updated.qs" file. On subsequent runs, it loads directly from
#' this updated cache when available.
#'
#' @param DOI A data frame or tibble with columns `Filename` (with extensions)
#'            and `DOI` for downloading missing files.
#'
#' @return Invisibly returns a named list of loaded datasets (base names).
#'         Each list element is the enriched data; updated caches are
#'         saved to "data/<base>_updated.qs".
#'
#' @examples
#' DOI <- tibble(
#'   Filename = c("dataset1.csv", "dataset2.rds"),
#'   DOI      = c("10.0000/abc", "10.0000/def")
#' )
#' loaded_list <- load_data(DOI)
#'
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom here here
#' @importFrom futile.logger flog.info
#' @importFrom readr read_csv cols col_guess
#' @importFrom qs qread qsave
#' @importFrom dplyr mutate
#' @importFrom lubridate year month
#' @importFrom sf st_read
#' @export
load_data <- function(DOI) {
  # Dependencies
  library(here)
  library(futile.logger)
  library(qs)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(sf)
  
  # Prepare enrichment tools
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  shp_raw       <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  
  # Storage for results
  data_dir <- here::here("data")
  
  for (i in seq_len(nrow(DOI))) {
    filename    <- DOI$Filename[i]
    doi_value   <- DOI$DOI[i]
    base_name   <- tools::file_path_sans_ext(filename)
    raw_path    <- file.path(data_dir, filename)
    cache_path  <- file.path(data_dir, paste0(base_name, "_updated.qs"))
    
    flog.info("Processing %s", filename)
    
    # Load from updated cache if exists
    if (file.exists(cache_path)) {
      flog.info("Loading cached updated .qs: %s", cache_path)
      data <- qread(cache_path)
    } else {
      # Ensure raw file present or download
      if (!file.exists(raw_path)) {
        flog.info("Downloading missing file: %s", filename)
        download_with_downloader(doi = doi_value, filename = filename)
      } else {
        flog.info("Raw file exists: %s", raw_path)
      }
      
      # Load raw data by extension
      ext <- tolower(tools::file_ext(filename))
      flog.info("Reading raw %s as %s", filename, ext)
      if (ext == "csv") {
        data <- read_csv(raw_path, col_types = cols(.default = col_guess()))
      } else if (ext == "rds") {
        data <- readRDS(raw_path)
      } else {
        warning("Unsupported extension: ", ext)
        next
      }
      
      # Type fixes
      if ("gear_type" %in% names(data)) data$gear_type <- as.character(data$gear_type)
      
      # Enrich and strip geometry
      data <- CWP.dataset::enrich_dataset_if_needed(data, shp_raw = shp_raw)$without_geom
      
      # Add temporal columns
      data <- data %>%
        dplyr::mutate(
          year  = year(time_start),
          month = month(time_start)
        )
      
      # Save updated cache
      qsave(data, cache_path)
      flog.info("Saved updated cache: %s", cache_path)
      
      # Option: remove raw CSV to save space
      if (ext == "csv") unlink(raw_path)
    }
    
  }
  
}
