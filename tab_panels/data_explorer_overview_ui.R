data_explorer_overview_ui <- function() {
  nav_panel("Dataset table exploring",
# hr(),
# textOutput("sql_query"),
hr(),
DT::dataTableOutput("DT"), 
hr(), 
sqlqueriesui()
# downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
)
}
