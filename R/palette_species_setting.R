
# Fonction pour initialiser les palettes basée sur les dataframes des variables cibles
initialiserPalettes <- function(targetVariables, seed=2643598) {
  # Fixer la graine pour la reproductibilité
  set.seed(seed)
  
  # Informations sur les palettes qualitatives disponibles
  paletteInfo <- brewer.pal.info[brewer.pal.info$category == "qual", ]
  
  # Créer une liste vide pour stocker les palettes finales
  palettes <- list()
  
  # Pour chaque dataframe dans targetVariables, générer une palette
  names(targetVariables) <- c("species", "fishing_fleet") # S'assurer que les noms correspondent
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
  # gridtype = target_gridtype,
  gear_type = target_gear_type
)

# Initialisation des palettes avec une graine fixe pour assurer la reproductibilité
palettes <- initialiserPalettes(targetVariables, seed=2643598) 

# Fonction pour récupérer une palette spécifique
getPalette <- function(category) {
  if (!is.null(palettes[[category]])) {
    return(palettes[[category]])
  } else {
    stop("Catégorie inconnue. Assurez-vous que la palette existe.")
  }
}
