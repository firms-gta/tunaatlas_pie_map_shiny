#' Initialize Color Palettes for Specific Variables
#'
#' This function generates unique color palettes for each specified variable based on the unique values
#' of each variable. It uses a fixed seed to ensure the reproducibility of the generated palettes.
#'
#' @param targetVariables A list of dataframes, each dataframe containing one column
#' with the unique values for a specific variable.
#' @param seed An integer used to set the seed of the random number generator,
#' ensuring the reproducibility of the generated palettes.
#'
#' @return A list of color vectors, where each vector corresponds to a color palette
#' for the specified variable.
#' @examples
#' target_species <- data.frame(species = c("Species 1", "Species 2", "Species 3"))
#' target_fishing_fleet <- data.frame(fishing_fleet = c("Fleet 1", "Fleet 2"))
#' targetVariables <- list(species = target_species, fishing_fleet = target_fishing_fleet)
#' palettes <- initialiserPalettes(targetVariables, seed=2643598)
#' getPalette("species")
#' @export
initialiserPalettes <- function(targetVariables, seed = 2643598) {
  # Fixer la graine pour la reproductibilité
  set.seed(seed)
  # Informations sur les palettes qualitatives disponibles
  paletteInfo <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == "qual", ]
  
  # Créer une liste vide pour stocker les palettes finales
  palettes <- list()
  
  # Pour chaque dataframe dans targetVariables, générer une palette
  for (variableName in names(targetVariables)) {
    dataframe <- targetVariables[[variableName]]
    # Le nom de la colonne unique dans chaque dataframe de targetVariables
    columnName <- names(dataframe)[1]
    
    # Identifier la longueur unique pour la variable
    nUnique <- nrow(dataframe)
    
    # Générer toutes les couleurs possibles pour les palettes qualitatives
    paletteAll <- unlist(mapply(RColorBrewer::brewer.pal, paletteInfo$maxcolors, rownames(paletteInfo)))
    
    flog.info("Generating palette for %s with %d unique values", variableName, nUnique)
    flog.info("Total available colors: %d", length(paletteAll))
    
    # Vérifier que nUnique est un entier positif
    if (!is.numeric(nUnique) || nUnique <= 0) {
      stop(sprintf("Invalid number of unique values for %s: %s", variableName, nUnique))
    }
    
    # Vérifier que length(paletteAll) est un entier positif
    if (!is.numeric(length(paletteAll)) || length(paletteAll) <= 0) {
      stop("Invalid length of available colors")
    }
    
    # S'assurer que nUnique ne dépasse pas la longueur de paletteAll
    if (nUnique > length(paletteAll)) {
      warning(sprintf("The number of unique values in %s exceeds the available palette size. Colors will be recycled.", variableName))
    }
    
    # Échantillonner parmi toutes les couleurs pour créer la palette spécifique
    palette <- sample(paletteAll, nUnique, replace = TRUE)
    # Ajuster la longueur de palette à celle de dataframe[[columnName]]
    palette <- rep(palette, length.out = nUnique)
    names(palette) <- dataframe[[columnName]]
    palette <- c(palette, Other = "#000000")
    
    flog.info("Generated palette for %s: %s", variableName, paste(palette, collapse = ", "))
    
    # Stocker la palette dans la liste des palettes
    palettes[[variableName]] <- palette
  }
  
  return(palettes)
}







