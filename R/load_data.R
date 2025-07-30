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

#' Charger, enrichir et mettre en cache les datasets
#'
#' @param DOI tibble avec colonnes `Filename` et `DOI`
#' @export
load_data <- function(DOI) {
  library(qs); library(readr); library(dplyr)
  library(lubridate); library(sf); library(futile.logger)
  library(tools); library(here)
  
  data_dir <- here::here("data")
  alt_data_dir <- path.expand("~/blue-cloud-dataspace/GlobalFisheriesAtlas/data_shiny_apps")
  
  for (i in seq_len(nrow(DOI))) {
    filename  <- DOI$Filename[i]
    doi_value <- DOI$DOI[i]
    base      <- file_path_sans_ext(filename)
    record_id <- sub(".*zenodo\\.([0-9]+)$", "\\1", doi_value)
    
    # 📁 Phase 0: check original file
    alt_path <- file.path(alt_data_dir, filename)
    local_target <- file.path(data_dir, filename)
    
    if (dir.exists(alt_data_dir) && file.exists(alt_path)) {
      message("📦 Found existing file in alternate path: ", alt_path)
      response <- readline(prompt = paste0("⏳ Do you want to use this file instead of downloading from Zenodo? [y/N]: "))
      if (tolower(response) == "y") {
        message("📁 Copying ", alt_path, " to ", local_target)
        file.copy(alt_path, local_target, overwrite = TRUE)
      } else {
        message("❌ Skipping local copy. Will proceed to download from Zenodo.")
      }
    }
    
    # 📥 Phase 1: download + convert to .qs
    renamed <- download_and_rename(doi_value, filename, data_dir)
    qs_path <- sub("\\.[^.]+$", ".qs", renamed)
    
    if (!file.exists(qs_path)) {
      ext <- tolower(file_ext(renamed))
      if (ext == "qs") {
        file.copy(renamed, qs_path)
      } else if (ext == "csv") {
        tbl <- read_csv(renamed, col_types = cols(.default = col_guess()))
        qs::qsave(tbl, qs_path)
      } else if (ext == "rds") {
        obj <- readRDS(renamed)
        qs::qsave(obj, qs_path)
      } else {
        warning("Unsupported extension: ", ext)
        next
      }
    } else {
      message("✅ .qs already exists: ", qs_path)
    }
    
    # 📥 Phase 2: load/copy/update enriched file
    cache_filename <- paste0(base, "_", record_id, "_updated.qs")
    cache_path <- file.path(data_dir, cache_filename)
    alt_cache_path <- file.path(alt_data_dir, cache_filename)
    
    if (dir.exists(alt_data_dir) && file.exists(alt_cache_path)) {
      message("📦 Found existing enriched file in alternate path: ", alt_cache_path)
      response <- readline(prompt = paste0("⏳ Do you want to use this enriched file instead of recomputing it? [y/N]: "))
      if (tolower(response) == "y") {
        message("📁 Copying enriched file ", alt_cache_path, " to ", cache_path)
        file.copy(alt_cache_path, cache_path, overwrite = TRUE)
        next  # skip enrichment
      } else {
        message("🔁 Skipping cached enriched copy. Proceeding with enrichment.")
      }
    }
    
    if (!file.exists(cache_path)) {
      message("✨ Enriching data from: ", qs_path)
      data_tbl <- qs::qread(qs_path)
      
      # fix types
      if ("gear_type" %in% names(data_tbl))
        data_tbl$gear_type <- as.character(data_tbl$gear_type)
      data_tbl$geographic_identifier <- as.character(data_tbl$geographic_identifier)
      
      # enrich
      require(CWP.dataset)
      require(tmap)
      data_tbl <- CWP.dataset::enrich_dataset_if_needed(data_tbl)$without_geom
      
      # simplify labels
      simplify_labels <- function(df) {
        label_cols <- grep("_label$", names(df), value = TRUE)
        for (label_col in label_cols) {
          base_col <- sub("_label$", "", label_col)
          if (base_col %in% names(df)) {
            df[[base_col]] <- paste(df[[base_col]], "-", df[[label_col]])
          }
        }
        df[ , !(names(df) %in% label_cols), drop = FALSE]
      }
      
      data_tbl <- simplify_labels(data_tbl)
      
      # add year/month
      data_tbl <- dplyr::mutate(data_tbl,
                                year  = year(time_start),
                                month = month(time_start))
      
      # save updated cache
      qs::qsave(data_tbl, cache_path)
      message("💾 Saved updated cache: ", cache_path)
    }
  }
  
  invisible(NULL)
}


