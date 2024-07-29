sidebar_ui_with_variable_to_display <- function(){
  sidebar(
    title = "Filter your data",
    useShinyjs(),
    uiOutput("year_input"),
    checkboxInput("toggle_year", "Discrete selection of year", value = FALSE),
    tags$br(),
    do.call(tagList, lapply(variable_to_display, function(variable) {
      if(variable == "species"){
        tagList(
          div(id = paste0(variable, "_toggle"), style = "cursor: pointer;", 
              HTML(sprintf("Select %s <span id='arrow_indicator'>&#9660;</span>", variable))),
          div(uiOutput("select_species")),
          div(class = "row", 
              div(class = "col-6", 
                  actionButton("all_species", "Select All Species")
              ),
              div(class = "col-6", 
                  actionButton("major_tunas", "Select Major Tunas")
              )
          ),
          tags$br(),
          tags$br()
        )
      } else {
        tagList(
          div(id = paste0(variable, "_toggle"), style = "cursor: pointer;", 
              HTML(sprintf("Select %s <span id='arrow_indicator'>&#9660;</span>", variable))),
          div(
            uiOutput(paste0("select_", variable)),
            actionButton(paste0("all_", variable), paste("Select All", gsub("_", " ", variable)))
          ),
          tags$br(),
          tags$br()
        )
      }
    })),
    div(class = "row", 
        div(class = "col-6", 
            actionButton("resetWkt", "Reset WKT to global")
        ),
        div(class = "col-6", 
            actionButton("resetFilters", "Reset Filters")
        )
    ),
    tags$br(),
    div(style = "position: -webkit-sticky; position: sticky; bottom: 0; z-index: 999;",
        actionButton("submit", "Submit", class = "btn-primary")
    ),
    tags$br(),
    tags$br(),
    actionButton("change_dataset", "Choose another dataset")
  )
}
