#' Load and Cache Data from Various File Formats
#'
#' This function loads datasets from various file formats (.qs, .csv, .qs) based on the filenames
#' provided in the `DOI` input. The data is loaded and cached in memory for future use. If the `.qs`
#' file is found, it is loaded first, otherwise, the function will try to load data from `.csv` or `.qs`
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
#' DOI <- data.frame(Filename = c("dataset1.csv", "dataset2.qs"))
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
#' @importFrom utils qs::qread
#' @export
load_data <- function(DOI) {
  loaded_data <- list()
  
  for (filename in DOI$Filename) {
    flog.info("Loading dataset: %s", filename)
    
    # Define file paths
    base_filename <- tools::file_path_sans_ext(filename)
    qs_file_path <- file.path('data', paste0(base_filename, '.qs'))
    csv_file_path <- file.path('data', paste0(base_filename, '.csv'))
    rds_file_path <- file.path('data', paste0(base_filename, '.qs'))
    
    # Check if .qs file exists
    if (file.exists(here::here(qs_file_path))) {
      flog.info("Load from qs")
      data <- qs::qread(here::here(qs_file_path))
      flog.info("Loaded %s from .qs", filename)
      loaded_data[[base_filename]] <- data
      
    } else {
      # If .qs does not exist, try to load from CSV or RDS
      if (file.exists(here::here(csv_file_path))) {
        # Load from CSV with specific column type
        data <- read_csv(here::here(csv_file_path), col_types = cols(gear_type = col_character()))
        flog.info("Loaded %s from CSV", filename)
        
      } else if (file.exists(here::here(rds_file_path))) {
        # Load from RDS
        data <- qs::qread(here::here(rds_file_path))
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
      file.remove(qs_file_path)
      qs::qsave(data, qs_file_path)
      flog.info("Saved %s as .qs", filename)
      
      # Add to the loaded_data list and assign to global environment
      loaded_data[[base_filename]] <- data
    }
    
    # Assign the loaded data to the global environment
    assign(base_filename, as.data.frame(loaded_data[[base_filename]]), envir = .GlobalEnv)
  }
}
