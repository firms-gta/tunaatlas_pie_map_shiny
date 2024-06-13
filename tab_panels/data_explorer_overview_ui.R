# Define the combined UI function with a grid layout
data_explorer_combined_ui <- function() {
  nav_panel("Dataset table exploring",
            grid_container(
              layout = c(
                "table1 table2"
              ),   
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              gap_size = "10px",
              
              # First table with title and download button in a single grid card
              grid_card(
                area = "table1",
                card_body(
                  card_header(
                    h3("CWP Standards Data"),
                    tags$p(a(href = "#", "What are CWP standards?", onclick = "alert('Navigating to CWP standards page');"))
                  ),
                  DT::dataTableOutput("DT"),
                  downloadButton("downloadCsvtableCWP", "Download CWP dataset as CSV"),
                  tags$br(), tags$br()
                )
              ),
              
              # Second table with title and download button in a single grid card
              grid_card(
                area = "table2",
                card_body(
                  card_header(
                    h3("Data by Species")
                  ),
                  DT::dataTableOutput("DTi11"),
                  downloadButton("downloadCsv", "Download as CSV"),
                  tags$br(), tags$br()
                )
              )
            ),
            hr(),
            sqlqueriesui()
  )
}
