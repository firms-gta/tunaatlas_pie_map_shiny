generateRmdNavMenu <- function(id, rmd_paths) {
  ns <- NS(id)
  panels <- lapply(seq_along(rmd_paths), function(i) {
    title <- tools::file_path_sans_ext(basename(rmd_paths[i]))
    nav_panel(title, uiOutput(ns(paste0("rmd_content_", i))))
  })
  do.call(nav_menu, c("More about", panels))
}


serveRmdContents <- function(id, rmd_paths) {
  moduleServer(id, function(input, output, session) {
    lapply(seq_along(rmd_paths), function(i) {
      output[[paste0("rmd_content_", i)]] <- renderUI({
        tags$iframe(src = rmd_paths[i], height = 600, width = "100%")
      })
    })
  })
}
