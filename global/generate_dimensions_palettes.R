flog.info(sprintf("Running generate_dimensions_palettes.R"))
flog.info(sprintf("Colnames default dataset %s:", colnames(default_dataset)))
# initialising variable to display
variable_to_display <- intersect(variable,colnames(default_dataset))
flog.info(sprintf("Colnames variable to display %s:", colnames(variable_to_display)))

# Create target_* variables for each non-numeric column
for (col in variable_to_display) {
  assign(paste0("target_", col), unique(default_dataset[[col]]))
  flog.info(sprintf("Target assigned %s:", col))
  
}

# Fonction pour générer les titres et IDs d'analyse
generate_analysis_option <- function(variable) {
  title <- paste0(toupper(substring(variable, 1, 1)), substring(variable, 2), " Analysis")
  id <- paste0(variable, "_analysis")
  list(title = title, id = id)
}

analysis_options <- lapply(variable_to_display, generate_analysis_option)

generate_dimension <- function(variable) {
  input_id <- paste0("select_", variable)
  column_name <- variable
  list(input_id = input_id, column_name = column_name)
}

# Générer les dimensions
dimensions <- lapply(variable_to_display, generate_dimension)

generate_target_variables <- function(variable) {
  target_name <- paste0("target_", variable)
  if (exists(target_name, envir = .GlobalEnv)) {
    return(base::get(target_name, envir = .GlobalEnv))
  } else {
    target_name <- paste0("target_", variable)
    target <- default_dataset%>%
      dplyr::select(!!sym(variable)) %>% 
      distinct()
    assign(target_name, target, envir = .GlobalEnv)
    return(target)
  }
}

targetVariables <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)

targetVariables2 <- lapply(targetVariables, as.data.frame)

targettes <- setNames(lapply(variable_to_display, generate_target_variables), variable_to_display)
# Initialize color palettes with a fixed seed for reproducibility
palettes <- initialiserPalettes(targetVariables2, seed = 2643598)
