render_rmd_to_html <- function(rmd_file, output_dir = "www") {
  html_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(rmd_file)), ".html"))
  
  # Check if the HTML file needs to be rendered
  if (!file.exists(html_file) || file.mtime(rmd_file) > file.mtime(html_file)) {
    rmarkdown::render(rmd_file, output_format = "html_document", output_file = basename(html_file), quiet = TRUE, output_dir = output_dir)
    flog.info(paste("Rendered HTML for", rmd_file))
  } else {
    flog.info(paste("Using cached HTML for", rmd_file))
  }
  
  return(html_file)
}


render_rmd_files <- function(rmd_files, output_dir = "www") {
  # Render all Rmd files to HTML if necessary
  html_files <- lapply(rmd_files, render_rmd_to_html, output_dir = output_dir)
  return(unlist(html_files))
}


# Define Rmd files to be rendered
rmd_files <- c(
  "rmd/Application_overview.Rmd",
  "rmd/Datasets.Rmd"
)

nav_bar_menu_rmd <- c(
  "rmd/Authors.Rmd", 
  "rmd/Fundings.Rmd", 
  "rmd/sidebar_explenations.Rmd", 
  "rmd/General_disclaimer.Rmd", 
  "rmd/Running_the_app.Rmd"
)

# Call the render function for the main and nav bar menu Rmd files
main_html_files <- render_rmd_files(rmd_files)
nav_bar_menu_html <- render_rmd_files(nav_bar_menu_rmd)
