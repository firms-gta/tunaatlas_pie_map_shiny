# This module should create multiple panels from a list of list variables 
# It does not work yet

# Function to create a navbarMenu with variable-specific insights
variables_nav_menu_ui <- function(id, variables) {
  ns <- NS(id)
  navbarMenu(
    title = "Variable Insights",
    lapply(variables, function(variable) {
      tabPanel(
        title = paste(variable, "Insights"),
        pieMapTimeSeriesUI(ns(paste0(variable, "_map"))),
        categoryGlobalPieChartUI(ns(paste0(variable, "_chart")))
      )
    })
  )
}



variables_nav_menu_server <- function(id, variables, sql_query, centroid) {
  moduleServer(id, function(input, output, session) {
    lapply(variables, function(variable) {
      ns <- session$ns
      
      # Initialize map module server
      pieMapTimeSeriesServer(ns(paste0(variable, "_map")),
                             category_var = tolower(variable), 
                             sql_query = sql_query, 
                             centroid = centroid)
      
      # Initialize chart module server
      categoryGlobalPieChartServer(ns(paste0(variable, "_chart")), 
                                   category = tolower(variable), 
                                   sql_query = sql_query)
    })
  })
}
