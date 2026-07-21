# Load required packages
lapply(c("here", "futile.logger", "readr", "tools", "sf", "dplyr", "data.table", "qs"), require, character.only = TRUE)
flog.info("Sourced create or load default dataset")

fallback_dataset_path <- here::here("inst", "extdata", "default_dataset.rds")

load_fallback_dataset <- function() {
  flog.warn("Utilisation du dataset minimal de secours (fallback versionne)")
  options(app_using_fallback_dataset = TRUE)
  readRDS(fallback_dataset_path)
}

variable <- c("species",
              "species_label",
              "fishing_fleet_label",
              "fishing_fleet",
              "source_authority",
              "FLEET_CODE",
              "GEAR_CODE",
              "SPECIES_CODE",
              "SCHOOL_TYPE_CODE",
              "CLASS_LOW",
              "CLASS_HIGH",
              "RAISE_CODE",
              "REPORTING_QUALITY",
              "gear_type_label",
              "gear_type",
              "species_name",
              "fishing_mode",
              "fishing_mode_label",
              "measurement_unit",
              "measurement_unit_label",
              "gridtype",
              "measurement",
              "measurement_type",
              "measurement_type_label",
              "species_group", 
              "species",
              "issue", 
              "species_group", 
              "Ocean", "ocean_basin","source_authority","AREA.CODE"
)

# Paths -------------------------------------------------------------------

doi_path <- here::here("DOI.csv")
data_cache_path <- here::here("data", "data.qs")

DOI <- tryCatch(
  {
    if (!file.exists(doi_path)) stop("DOI.csv introuvable")
    d <- readr::read_csv(doi_path, show_col_types = FALSE,
                         col_types = readr::cols(.default = readr::col_character()))
    if (nrow(d) == 0) stop("DOI.csv vide")
    d
  },
  error = function(e) {
    flog.warn("Probleme avec DOI.csv (%s) : bascule sur dataset minimal", e$message)
    NULL
  }
)

if (is.null(DOI)) {
  data <- load_fallback_dataset()
} else {
  options(app_using_fallback_dataset = FALSE)
# The first DOI.csv row is always the default dataset
default_doi <- DOI$DOI[[1]]
default_filename <- DOI$Filename[[1]]

record_id <- sub(
  ".*zenodo\\.([0-9]+)$",
  "\\1",
  default_doi
)

default_extension <- tolower(
  tools::file_ext(default_filename)
)

default_basename <- tools::file_path_sans_ext(
  default_filename
)

package_version <- if (
  requireNamespace("CWP.dataset", quietly = TRUE)
) {
  as.character(utils::packageVersion("CWP.dataset"))
} else {
  "not-installed"
}

# Increment this manually when the cache structure changes
cache_version <- "3"

cache_key <- paste(
  cache_version,
  default_doi,
  default_filename,
  package_version,
  sep = "|"
)

flog.info(
  "Default DOI dataset: %s",
  default_filename
)

# Cache validation --------------------------------------------------------

is_valid_data_cache <- function(x, expected_key) {
  required_fields <- c(
    "initial_data",
    "data_for_filters",
    "palettes",
    "targettes",
    "variable_to_display",
    "cache_key"
  )
  
  !is.null(x) &&
    !is.data.frame(x) &&
    is.list(x) &&
    all(required_fields %in% names(x)) &&
    identical(as.character(x$cache_key), expected_key) &&
    is.data.frame(x$data_for_filters) &&
    nrow(x$data_for_filters) > 0 &&
    length(x$variable_to_display) > 0
}

data <- NULL

if (file.exists(data_cache_path)) {
  cached_data <- tryCatch(
    qs::qread(data_cache_path),
    error = function(e) {
      flog.warn(
        "Unable to read data.qs: %s",
        e$message
      )
      NULL
    }
  )
  
  if (is_valid_data_cache(cached_data, cache_key)) {
    data <- cached_data
    
    flog.info(
      "Valid data.qs loaded for %s",
      default_filename
    )
  } else {
    flog.warn(
      "data.qs is obsolete or invalid and will be rebuilt"
    )
  }
}

# Default dataset loading -------------------------------------------------

read_default_dataset <- function(path) {
  extension <- tolower(tools::file_ext(path))
  
  result <- switch(
    extension,
    qs = qs::qread(path),
    
    csv = readr::read_csv(
      path,
      show_col_types = FALSE,
      progress = FALSE
    ),
    
    rds = readRDS(path),
    
    stop(
      "Unsupported default dataset extension: ",
      extension
    )
  )
  
  if (!is.data.frame(result)) {
    stop(
      "The default dataset is not a data.frame or sf object: ",
      path
    )
  }
  
  if (!"geographic_identifier" %in% names(result)) {
    stop(
      "The default dataset has no geographic_identifier column"
    )
  }
  
  result$geographic_identifier <-
    as.character(result$geographic_identifier)
  
  result
}

if (is.null(data)) {
  data <- tryCatch({
  source(here::here("R", "data_loading.R"))
  source(
    here::here(
      "global",
      "generate_dimensions_palettes.R"
    )
  )
  
  # Prefer an already enriched/converted QS file
  possible_paths <- c(
    here::here(
      "data",
      paste0(
        default_basename,
        "_",
        record_id,
        "_updated.qs"
      )
    ),
    here::here(
      "data",
      paste0(
        default_basename,
        "_",
        record_id,
        ".qs"
      )
    ),
    here::here(
      "data",
      paste0(
        default_basename,
        "_",
        record_id,
        ".",
        default_extension
      )
    ),
    here::here(
      "data",
      default_filename
    )
  )
  
  valid_paths <- possible_paths[
    file.exists(possible_paths) &
      file.info(possible_paths)$size > 0
  ]
  
  if (length(valid_paths) > 0) {
    default_path <- valid_paths[[1]]
    
    flog.info(
      "Using existing default dataset: %s",
      default_path
    )
  } else {
    flog.info(
      "Default dataset is missing; downloading first DOI.csv row"
    )
    
    default_path <- download_and_rename(
      doi = default_doi,
      filename = default_filename,
      data_dir = here::here("data")
    )
  }
  
  if (
    is.null(default_path) ||
    !file.exists(default_path) ||
    file.info(default_path)$size == 0
  ) {
    stop(
      "Unable to retrieve the first DOI.csv dataset: ",
      default_filename
    )
  }
  
  default_dataset <- read_default_dataset(
    default_path
  )
  
  # FishStat-specific preparation
  if ("AREA.CODE" %in% names(default_dataset)) {
    Sys.setenv(APP_ENABLE_MAP = "0")
    
    flog.info("Preparing FishStat dataset")
    
    default_dataset <- default_dataset %>%
      dplyr::mutate(
        year_chr = stringr::str_trim(
          as.character(year)
        ),
        date = suppressWarnings(
          lubridate::ymd(year_chr)
        ),
        date = dplyr::if_else(
          is.na(date),
          as.Date(year_chr),
          date
        ),
        year = lubridate::year(date),
        month = 1
      )
    
    variable <- setdiff(
      variable,
      c("species", "source_authority")
    )
  }
  
  # Structure expected by server.R
  data <- load_initial_data(
    default_dataset
  )
  
  dimensions <- generate_dimensions_palettes(
    df = data$data_for_filters,
    variable = variable,
    seed = 2643598
  )
  
  data$palettes <- dimensions$palettes
  data$targettes <- dimensions$targettes
  data$variable_to_display <- dimensions$variables
  
  # Cache metadata
  data$cache_key <- cache_key
  data$cache_version <- cache_version
  data$default_doi <- default_doi
  data$default_filename <- default_filename
  
  dir.create(
    dirname(data_cache_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  qs::qsave(
    data,
    data_cache_path
  )
  
  flog.info(
    "data.qs created: %s rows; variables=%s",
    nrow(data$data_for_filters),
    paste(
      data$variable_to_display,
      collapse = ", "
    )
  )
  data
  }, error = function(e){
    flog.warn("Echec du chargement/enrichissement du dataset reel (%s) : bascule sur minimal", e$message)
    load_fallback_dataset()
  })
}
# Compatibility with existing application code
default_dataset <- data$data_for_filters
}