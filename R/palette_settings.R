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
initialiserPalettes <- function(targetVariables, seed=2643598) {
  # Fixer la graine pour la reproductibilité
  set.seed(seed)
  
  # Informations sur les palettes qualitatives disponibles
  paletteInfo <- brewer.pal.info[brewer.pal.info$category == "qual", ]
  
  # Créer une liste vide pour stocker les palettes finales
  palettes <- list()
  
  # Pour chaque dataframe dans targetVariables, générer une palette
  names(targetVariables) <- c("species", "fishing_fleet", "gear_type") # S'assurer que les noms correspondent
  for (variableName in names(targetVariables)) {
    dataframe <- targetVariables[[variableName]]
    # Le nom de la colonne unique dans chaque dataframe de targetVariables
    columnName <- names(dataframe)[1]
    
    # Identifier la longueur unique pour la variable
    nUnique <- nrow(dataframe)
    
    # Générer toutes les couleurs possibles pour les palettes qualitatives
    paletteAll <- unlist(mapply(brewer.pal, paletteInfo$maxcolors, rownames(paletteInfo)))
    
    # Échantillonner parmi toutes les couleurs pour créer la palette spécifique
    palette <- sample(paletteAll, nUnique, replace=TRUE)
    names(palette) <- dataframe[[columnName]]
    
    # Stocker la palette dans la liste des palettes
    palettes[[variableName]] <- palette
  }
  
  return(palettes)
}

# Préparation de la liste targetVariables
targetVariables <- list(
  species = target_species,
  fishing_fleet = target_flag,
  gear_type = target_gear_type
)

# Initialisation des palettes avec une graine fixe pour assurer la reproductibilité
palettes <- initialiserPalettes(targetVariables, seed=2643598) 

#' Retrieve a Specific Color Palette
#'
#' This function returns the color palette for the specified category, if it exists.
#'
#' @param category The name of the category for which the color palette is requested.
#'
#' @return A color vector for the specified category.
#' @examples
#' palette_species <- getPalette("species")
#' @export
#' 
getPalette <- function(category) {
  if (!is.null(palettes[[category]])) {
    return(palettes[[category]])
  } else {
    stop("Unknown category. Be sure of the existance of this palette.")
  }
}
