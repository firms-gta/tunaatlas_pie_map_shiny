require(readr)

# Read the DOI file
DOI <- read_csv("data/DOI.csv")

# Set a global timeout for downloads
options(timeout = 6000)

# Function to download files using curl
download_with_curl <- function(doi, filename, data_dir = "data") {
  # Ensure the data directory exists
  if (!dir.exists(data_dir)) dir.create(data_dir)
  
  # Construct the URL from the DOI
  url <- paste0("https://doi.org/", doi)
  
  # Destination file path
  destfile <- file.path(data_dir, filename)
  
  # Attempt the download
  tryCatch({
    curl::curl_download(url = url, destfile = destfile)
    
    # Check if the file was successfully downloaded
    if (file.exists(destfile)) {
      message(sprintf("File '%s' downloaded successfully with curl", filename))
    } else {
      stop(sprintf("File '%s' could not be downloaded with curl", filename))
    }
  }, error = function(e) {
    message(sprintf("Curl download failed for file '%s': %s", filename, e$message))
    stop(sprintf("Failed to download file '%s' from DOI: %s", filename, doi))
  })
}

# Download all files from the DOI list
lapply(1:nrow(DOI), function(i) {
  filepath <- file.path("data", DOI$Filename[i])
  
  # Check if the file already exists
  if (!file.exists(filepath)) {
    download_with_curl(doi = DOI$DOI[i], filename = DOI$Filename[i])
  } else {
    message(sprintf("File '%s' already exists. Skipping download.", DOI$Filename[i]))
  }
})
