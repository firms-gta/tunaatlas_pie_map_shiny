
require(zen4R)
require(readr)

DOI <- read_csv("data/DOI.csv")

options(timeout = 600)

extract_zenodo_metadata <- function(doi, filename, data_dir = "data") {
  
  dir <- getwd()
  # Create the data directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  # Set the working directory to the data directory
  setwd(data_dir)
  
  # Export DublinCore metadata
  zen4R::export_zenodo(doi = doi, filename = "zenodoDublincore", format = "DublinCore")
  
  # Download the required file
  zen4R::download_zenodo(doi = doi, files = filename)
  
  
  # Reset the working directory to the original
  setwd(dir)
}

lapply(1:nrow(DOI), function(i) {
  if (!file.exists(paste0("data/", DOI$Filename[i]))) {
    extract_zenodo_metadata(doi = DOI$DOI[i], filename = DOI$Filename[i])
  }
})



