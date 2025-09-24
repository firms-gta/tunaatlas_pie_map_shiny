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
  renamedinqs     <- file.path(data_dir, paste0(base, "_", record_id, ".qs"))
  updated     <- file.path(data_dir, paste0(base, "_", record_id, "_updated.qs"))
  
  # 1) Si déjà renommé, on ne fait rien
  if (file.exists(renamed) | file.exists(updated) | file.exists(renamedinqs)) {
    
    invisible(lapply(c(renamed, updated, renamedinqs), function(f) {
      if (file.exists(f)) message("📦 found file: ", f)
    }))
    
    if (file.exists(renamedinqs)){
      return(renamedinqs)
    } 
    
    if(file.exists(renamed) | file.exists(updated)){
    return(renamed)
      }
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
cl_areal_grid_path <- here::here("data/cl_areal_grid.qs")


if(!file.exists(cl_areal_grid_path)| !file.exists("data/centroids.qs")){
  cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
  if (!file.exists(cwp_grid_file) ) {
    stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
  }
  library(dplyr)
  shp_raw <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  shapefile.fix <- sf::st_as_sf(shp_raw, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 geom     = geom_wkt) %>% 
    dplyr::select(geom, geographic_identifier = code, gridtype = GRIDTYPE) %>% dplyr::distinct()
  pts   <- sf::st_point_on_surface(shapefile.fix)
  cr    <- sf::st_coordinates(pts)
  shapefile.fix <- bind_cols(shapefile.fix, as.data.frame(cr))
  centroids <- shapefile.fix %>% sf::st_drop_geometry() %>% dplyr::select(-gridtype)
  
  qs::qsave(shapefile.fix, cl_areal_grid_path)
  qs::qsave(centroids, "data/centroids.qs")
} else {
  shapefile.fix <- qs::qread(cl_areal_grid_path)
  centroids <- qs::qread("data/centroids.qs")
}
require(here)
source(here::here("R/load_data.R"))
if(!exists("default_dataset")){
  load_data(DOI) 
}




