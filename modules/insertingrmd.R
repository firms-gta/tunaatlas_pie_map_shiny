#' UI Function for Inserting Rmd Content
#'
#' This function creates a UI element for displaying rendered R Markdown files.
#' It generates a UI output placeholder that will be filled with the HTML content
#' of a rendered R Markdown file.
#'
#' @param id A unique identifier for the UI element to ensure proper namespace handling in Shiny modules.
#'
#' @return A UI output that serves as a placeholder for the R Markdown content.
#' @export
#'
#' @examples
#' # In the UI part of a Shiny app
#' instertingrmdUI("uniqueID")
instertingrmdUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("funding_content"))
}

#' Server Function for Inserting Rmd Content
#'
#' This function handles the server-side logic for rendering and displaying
#' R Markdown files in a Shiny application. It takes the path of an R Markdown file,
#' renders it to HTML, and then displays it within an iframe.
#'
#' @param id A unique identifier matching the UI element's ID for namespace handling.
#' @param path The file path to the R Markdown document to be rendered and displayed.
#'
#' @return None.
#'
#' @export
#'
#' @examples
#' # In the server part of a Shiny app, assuming `instertingrmdUI` has been called with "uniqueID"
#' instertingrmdServer("uniqueID", "path/to/document.Rmd")
instertingrmdServer <- function(id, path) {
  moduleServer(id, function(input, output, session) {
    output$funding_content <- renderUI({
      req(path) # Ensure path is provided
      # Read and parse the Rmd file
      rmd_content <- rmarkdown::render(path, quiet = TRUE)
      # Embed the HTML content
      tags$iframe(srcdoc = rmd_content, height = 600, width = "100%")
    })
  })
}
