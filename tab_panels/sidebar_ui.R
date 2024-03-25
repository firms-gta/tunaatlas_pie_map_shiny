sidebar_ui <- function(){
  sidebar(
    title = "Filter your data",
    uiOutput("select_dataset"), 
    uiOutput("select_gridtype"),
    uiOutput("select_species"),
    div(class = "row", 
        div(class = "col-6", 
            actionButton("all_species", "Select All Species")
        ),
        div(class = "col-6", 
            actionButton("major_tunas", "Select Major Tunas")
        )
    ),
    tags$br(),
    tags$br(),
    uiOutput("select_fishing_fleet"),
    actionButton("all_fishing_fleet", "Select All Fishing Fleets"),
    tags$br(),
    tags$br(),
    uiOutput("select_year"),
    tags$br(),
    tags$br(),
    div(class = "row", 
        div(class = "col-6", 
            actionButton("resetWkt", "Reset WKT to global")
        ),
        div(class = "col-6", 
            actionButton("resetFilters", "Reset Filters")
        )
    ),
    tags$br(),
    tags$br(),
    actionButton("submit", "Submit"),
  )
}