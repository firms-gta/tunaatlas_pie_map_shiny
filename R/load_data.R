#' Load and Cache Data from Various File Formats
#'
#' This function loads datasets from various file formats (.qs, .csv, .rds) based on the filenames
#' provided in the `DOI` input. The data is loaded and cached in memory for future use. If the `.qs`
#' file is found, it is loaded first, otherwise, the function will try to load data from `.csv` or `.rds`
#' formats. It also saves the data in `.qs` format after loading to speed up future accesses.
#'
#' @param DOI A data frame or tibble with at least one column `Filename` containing the filenames (with extensions)
#'            of the datasets to be loaded.
#'
#' @return The function does not return any value but assigns the loaded data to the global environment.
#'         Each dataset is stored in a variable with the base name of the file (without extension).
#'
#' @examples
#' # Assuming `DOI` is a data frame with filenames to load
#' DOI <- data.frame(Filename = c("dataset1.csv", "dataset2.rds"))
#' load_data(DOI)
#'
#' # After calling the function, datasets will be available in the global environment
#' # with names corresponding to the base filenames (e.g., dataset1, dataset2)
#' # The data will be loaded and saved in `.qs` format for future access.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom here here
#' @importFrom flog flog.info
#' @importFrom readr read_csv cols col_character
#' @importFrom qs qread qsave
#' @importFrom base warning next assign
#' @importFrom utils readRDS
#' @export
load_data <- function(DOI) {
  # Load datasets from DOI list efficiently
  loaded_data <- list()
  
  for (i in seq_len(nrow(DOI))) {
    filename <- DOI$Filename[i]
    base_filename <- tools::file_path_sans_ext(filename)
    qs_file_path <- here::here("data", paste0(base_filename, ".qs"))
    csv_file_path <- here::here("data", paste0(base_filename, ".csv"))
    rds_file_path <- here::here("data", paste0(base_filename, ".rds"))
    filepath <- here::here("data", filename)
    
    flog.info("Processing dataset: %s", filename)
    
    # If .qs exists, load it and skip downloading
    if (file.exists(qs_file_path)) {
      flog.info("Loading from existing .qs file: %s", filename)
      data <- qs::qread(qs_file_path)
      
    } else {
      # If .qs doesn't exist, ensure file is downloaded
      if (!file.exists(filepath)) {
        flog.info("Downloading file: %s", filename)
        download_with_downloader(doi = DOI$DOI[i], filename = filename)
      } else {
        flog.info("File already exists, skipping download: %s", filename)
      }
      
      # Load from CSV or RDS
      if (file.exists(csv_file_path)) {
        data <- read_csv(csv_file_path, col_types = cols(gear_type = col_character()))
        flog.info("Loaded from CSV: %s", filename)
        
        # Save as .qs and delete CSV
        qs::qsave(data, qs_file_path)
        flog.info("Saved as .qs and deleted CSV: %s", filename)
        unlink(csv_file_path)
        
      } else if (file.exists(rds_file_path)) {
        data <- readRDS(rds_file_path)
        flog.info("Loaded from RDS: %s", filename)
        
        # Ensure gear_type is character
        if ("gear_type" %in% names(data)) {
          data$gear_type <- as.character(data$gear_type)
        }
        
        # Save as .qs
        qs::qsave(data, qs_file_path)
        flog.info("Saved as .qs: %s", filename)
        
      } else {
        warning(sprintf("File not found for %s: neither CSV nor RDS exists.", filename))
        next
      }
    }
    
    # Assign data to global environment
    loaded_data[[base_filename]] <- data
    assign(base_filename, as.data.frame(data), envir = .GlobalEnv)
  }
}


