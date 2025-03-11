require(readr)
require(utils)
require(downloader)

# Read the DOI file
DOI <- read_csv("DOI.csv")

# Set a global timeout for downloads
options(timeout = 6000)

# Function to download files using curl
download_with_downloader <- function(doi, filename, data_dir = "data") {
  # S'assurer que le répertoire des données existe
  if (!dir.exists(data_dir)) dir.create(data_dir)
  
  record_id <- sub("10\\.5281/zenodo\\.", "", doi)
  
  # Construire l'URL brute
  raw_url <- paste0("https://zenodo.org/record/", record_id, "/files/", filename, "?download=1")
  
  # Encoder les caractères spéciaux dans l'URL
  encoded_url <- URLencode(raw_url)
  
  # Chemin de destination
  destfile <- file.path(data_dir, filename)
  
  # Télécharger le fichier
  tryCatch({
    downloader::download(encoded_url, destfile, mode = "wb")
    message(sprintf("File '%s' downloaded successfully.", filename))
    
    
    # Vérifier si le fichier a été correctement téléchargé
    if (file.exists(destfile)) {
      message(sprintf("File '%s' downloaded successfully from DOI: %s", filename, doi))
    } else {
      stop(sprintf("File '%s' could not be downloaded from DOI: %s", filename, doi))
    }
  }, error = function(e) {
    message(sprintf("Curl download failed for file '%s': %s", filename, e$message))
    stop(sprintf("Failed to download file '%s' from DOI: %s", filename, doi))
  })
}

source(here::here("R/load_data.R"))
load_data(DOI)