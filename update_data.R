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
  if (!dir.exists(data_dir)) dir.create(data_dir)
  setwd(data_dir)
  
  success <- FALSE
  attempts <- 3
  attempt <- 1
  
  while (!success && attempt <= attempts) {
    tryCatch({
      zen4R::export_zenodo(doi = doi, filename = "zenodoDublincore", format = "DublinCore")
      
      zen4R::download_zenodo(doi = doi, files = filename)
      
      # Check if the file was downloaded
      if (file.exists(filename)) {
        success <- TRUE
        message(sprintf("File '%s' downloaded successfully", filename))
      } else {
        message(sprintf("File '%s' was not downloaded completely, retrying...", filename))
      }
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed for file '%s': %s", attempt, filename, e$message))
    })
    attempt <- attempt + 1
  }
  
  setwd(dir)
}

# Use the function with lapply for each DOI
lapply(1:nrow(DOI), function(i) {
  filepath <- paste0("data/", DOI$Filename[i])
  if (!file.exists(filepath)) {
    extract_zenodo_metadata(doi = DOI$DOI[i], filename = DOI$Filename[i])
  }
})
