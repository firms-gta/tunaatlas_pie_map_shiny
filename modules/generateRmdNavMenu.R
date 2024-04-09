#' Generate R Markdown Navigation Menu
#'
#' Dynamically creates a navigation menu for R Markdown documents in a Shiny application. 
#' This function constructs a list of panels, each representing a link to an R Markdown document, 
#' which are then passed to a navigation menu UI component.
#'
#' @param id A unique identifier for the navigation menu, used for namespace scoping.
#' @param rmd_paths A character vector of file paths to R Markdown documents.
#'
#' @return A UI object for inclusion in a Shiny application UI definition.
#' @export
#'
#' @examples
#' \dontrun{
#' generateRmdNavMenu("my_rmd_nav", c("/path/to/Document1.Rmd", "/path/to/Document2.Rmd"))
#' }

generateRmdNavMenu <- function(id, rmd_paths) {
  ns <- NS(id)
  panels <- lapply(seq_along(rmd_paths), function(i) {
    title <- get_html_title(rmd_paths[i])
    nav_panel(title, uiOutput(ns(paste0("rmd_content_", i))))
  })
  do.call(nav_menu, c("More about", panels))
}

#' Serve R Markdown Document Contents
#'
#' Sets up server logic for rendering and displaying R Markdown documents in a Shiny module. 
#' Each document is displayed in an iframe within the Shiny app, allowing for seamless integration.
#'
#' @param id A unique identifier for the module, corresponding to the ID used in `generateRmdNavMenu`.
#' @param rmd_paths A character vector of file paths to R Markdown documents, matching those passed to `generateRmdNavMenu`.
#' In this shiny these character vector is defined by the list_markdown_path variable in the rmd/rendering_rmd_files_to_html.R script
#'
#' @details This function should be called inside a moduleServer call in the server part of a Shiny module.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' serveRmdContents("my_rmd_nav", c("/path/to/Document1.Rmd", "/path/to/Document2.Rmd"))
#' }
#' 
serveRmdContents <- function(id, rmd_paths) {
  moduleServer(id, function(input, output, session) {
    lapply(seq_along(rmd_paths), function(i) {
      output[[paste0("rmd_content_", i)]] <- renderUI({
        tags$iframe(src = rmd_paths[i], height = 600, width = "100%")
      })
    })
  })
}
