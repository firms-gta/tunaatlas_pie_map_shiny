generate_dimensions_palettes <- function(df, variable, seed = 2643598) {
  requireNamespace("dplyr")
  requireNamespace("futile.logger")
  
  flog.info("Running generate_dimensions_palettes()")
  flog.info(sprintf("Colnames dataset: %s", paste(colnames(df), collapse = ", ")))
  
  # Variables à afficher (intersection entre "variable" et les colonnes du df)
  variable_to_display <- intersect(variable, colnames(df))
  flog.info(sprintf("Variables to display: %s", paste(variable_to_display, collapse = ", ")))
  
  # Générer les cibles (targettes)
  generate_target_variables <- function(var) {
    unique(df[[var]])
  }
  targettes <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)
  
  # Conversion en data.frame pour initialiser les palettes
  targetVariables2 <- lapply(targettes, as.data.frame)
  
  # Générer les palettes avec un seed fixe pour reproductibilité
  source(here::here("R/initialiserPalettes.R"))
  palettes <- initialiserPalettes(targetVariables2, seed = seed)
  flog.info("Palettes successfully generated")
  
  # Retourner une liste propre
  return(list(
    palettes = palettes,
    targettes = targettes,
    variables = variable_to_display
  ))
}