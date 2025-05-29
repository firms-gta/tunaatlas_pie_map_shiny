require(readr)
require(utils)
require(downloader)

# Read the DOI file
DOI <- readr::read_csv("DOI.csv")

# Set a global timeout for downloads
options(timeout = 6000)

# Function to download files using curl
#' Télécharger et renommer un fichier depuis Zenodo
#'
#' @param doi    Le DOI Zenodo (ex. "10.5281/zenodo.1234567")
#' @param filename Nom de fichier attendu (ex. "data.csv")
#' @param data_dir  Répertoire cible (par défaut "data")
#' @return Chemin vers le fichier renommé (avant conversion .qs)
download_and_rename <- function(doi, filename, data_dir = "data") {
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  record_id <- sub(".*zenodo\\.([0-9]+)$", "\\1", doi)
  ext       <- tools::file_ext(filename)
  base      <- tools::file_path_sans_ext(filename)
  
  raw_path    <- file.path(data_dir, filename)
  renamed     <- file.path(data_dir, paste0(base, "_", record_id, ".", ext))
  
  # 1) Si déjà renommé, on ne fait rien
  if (file.exists(renamed)) {
    message("📦 found renamed file: ", renamed)
    return(renamed)
  }
  
  # 2) Si le brut existe, on le renomme
  if (file.exists(raw_path)) {
    message("📦 copying local file → ", renamed)
    file.copy(raw_path, renamed)
    return(renamed)
  }
  
  # 3) Sinon on download
  url <- sprintf("https://zenodo.org/record/%s/files/%s?download=1",
                 sub(".*zenodo\\.([0-9]+)$", "\\1", doi),
                 filename)
  message("🌐 downloading ", filename)
  tryCatch({
    downloader::download(URLencode(url), raw_path, mode = "wb")
    if (!file.exists(raw_path)) stop("Download failed")
    file.rename(raw_path, renamed)
    message("✅ downloaded & renamed → ", renamed)
    return(renamed)
  }, error = function(e) {
    stop("Failed to download '", filename, "': ", e$message)
  })
}

require(here)
source(here::here("R/load_data.R"))
load_data(DOI)



