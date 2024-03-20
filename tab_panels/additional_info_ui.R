# Define the UI for the "Additional Information" tab
additional_info_ui <- function() {
  navbarMenu("More",
             tabPanel("SQL Queries",
                      div(class="outer",
                          h3("SQL Queries Overview"),
                          p("Here you can find the SQL queries used to retrieve the data displayed in this application."),
                          title = "Your SQL query for overview",
                          textOutput("sql_query_metadata"),
                          title = "Your SQL query",
                          textOutput("sql_query"),
                          title = "Your SQL query plot1",
                          textOutput("sql_query_metadata_plot1")
                          
                          
                      )
             ),
             tabPanel("About",
                      fluidRow(
                        column(6,
                               includeMarkdown("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/README.md")
                        ),
                        column(3,
                               img(class="logo_IRD",
                                   src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                               tags$small(
                                 "Source: IRD",
                                 "Julien Barde ",
                                 "Funding : BlueCloud ",
                                 a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                   "IRD Tuna Atlas (Alain Fontenau)"),
                                 a(href="https://github.com/juldebar/IRDTunaAtlas/wiki/Indicator-I11-:-Catches-by-fishing_fleet",
                                   "IRD Indicator 11"),
                                 a(href="https://www.documentation.ird.fr/hor/fdi:010012425",
                                   "IRD Tuna Atlas (Alain Fontenau)"),
                                 a(href="https://horizon.documentation.ird.fr/exl-doc/pleins_textes/divers11-03/010012425.pdf",
                                   "PDF")
                               )
                        ),
                        column(3,
                               img(class="logo_IRD",
                                   src=paste0("https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg")),
                               tags$small(
                                 "General Disclaimer:",
                                 "This repository contains work in progress. It can be used to explore the content of multi-dimensionnal data cubes storing tuna fisheries data. Dimensions are: spatial (lat,lon), time, flag of the fishing fleet, free schools or FADs. The content is made of publicly available data delivered by Tuna RFMOs. Its content should not be used for publications without explicit agreement from the authors. The accuracy of the estimates provided in these analyses is contingent on data quality and availability. Results presented here do not represent the official view of IRD, its staff or consultants.",
                                 "Caution must be taken when interpreting all data presented, and differences between information products published by IRD and other sources using different inclusion criteria and different data cut-off times are to be expected. While steps are taken to ensure accuracy and reliability, all data are subject to continuous verification and change.  See here for further background and other important considerations surrounding the source data."
                               )
                        )
                        
                      )
  )
  )
}
