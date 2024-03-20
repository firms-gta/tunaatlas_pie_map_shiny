filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
    h2("Select filters to customize indicators"),
    selectInput(ns("dataset"), "Dataset", choices = NULL),
    selectInput(ns("gridtype"), "Resolution", choices = NULL),
    sliderInput(ns("year"), "Select period of interest:", min = 1950, max = 2025, value = c(1950, 2025)),
    tags$br(),
    selectInput(ns("species"), "Species", choices = NULL, multiple = TRUE),
    actionButton("all_species", "Select All Species"),
    tags$br(),
    selectInput(ns("fishing_fleet"), "Fishing Fleet", choices = NULL, multiple = TRUE),
    actionButton("all_fishing_fleet", "Select All Fishing Fleets"),
    tags$br(),
    actionButton(ns("submit"), "Submit"),
    tags$br(),
    tags$br(),
    actionButton(ns("resetWkt"), "Reset WKT to global"),
    actionButton(ns("resetFilters"), "Reset Filters"),
    tags$br(),
    tags$br()
  )
}
