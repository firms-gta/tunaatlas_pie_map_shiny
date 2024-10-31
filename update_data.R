require(zen4R)
require(readr)
DOI <- read_csv("data/DOI.csv")

options(timeout = 6000) # Global timeout for downloads

# Function to verify file size (if available in DOI)
verify_file <- function(filepath, expected_size) {
  file.info(filepath)$size == expected_size
}

extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  dir <- getwd()
  # Create data directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  # Set working directory to the data directory
  setwd(data_dir)
  
  # Export DublinCore metadata
  zen4R::export_zenodo(doi = doi, filename = "zenodoDublincore", format = "DublinCore")
  
  # Download the file with retry logic
  success <- FALSE
  attempts <- 3
  attempt <- 1
  
  while (!success && attempt <= attempts) {
    tryCatch({
      zen4R::download_zenodo(doi = doi, files = filename)
      
      # Check if the file was downloaded and matches the expected size
      if (file.exists(filename)) {
        # If `Size` exists in DOI, we can compare the size
        success <- is.na(DOI$Size[DOI$Filename == filename]) ||
          verify_file(filename, expected_size = DOI$Size[DOI$Filename == filename])
      }
      
      if (success) {
        message(sprintf("File '%s' downloaded successfully", filename))
      } else {
        message(sprintf("File '%s' is incomplete, retrying...", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed for file '%s': %s", attempt, filename, e$message))
    })
    attempt <- attempt + 1
  }
  
  # Reset working directory
  setwd(dir)
}

# Use the function with lapply for each DOI
lapply(1:nrow(DOI), function(i) {
  filepath <- paste0("data/", DOI$Filename[i])
  if (!file.exists(filepath)) {
    zen4R::extract_zenodo_metadata(doi = DOI$DOI[i], filename = DOI$Filename[i])
  }
})
