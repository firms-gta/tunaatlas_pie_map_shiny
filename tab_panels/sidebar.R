sidebar <- div(
  class = "outer",
  tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, right = 20, width = 300, height = "auto",
                tags$br(),
                h2("Select filters to customize indicators"),
                
                selectInput("dataset", "Dataset", choices = NULL),
                selectInput("gridtype", "Resolution", choices = NULL, multiple = TRUE),
                selectInput("species", "Species", choices = NULL, multiple = TRUE),
                sliderInput("year", "Select period of interest:", min = 1950, max = 2025, value = c(1950, 2025)),
                selectInput("fishing_fleet", "Fishing Fleet", choices = NULL, multiple = TRUE),
                
                actionButton("submit", "Submit"),
                actionButton("resetWkt", "Reset WKT to global"),
                actionButton("resetFilters", "Reset Filters"),
                
                tags$br(),
                tags$br())
)
