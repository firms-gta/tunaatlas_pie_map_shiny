geographic_catches_by_fishing_fleet_ui <- function() {
  tabPanel("Interactive Indicator 11 by fishing fleet",
           filterUI("interactive_tab"),
  div(class="outer",
      tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
      leafletOutput("map_i11", width="100%", height="100%"),
      
      
      absolutePanel(id = "controls", class = "panel panel-default",
                    top = 200, left = "auto", width = "20%", fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    tags$br(),
                    plotOutput("pie_map_i11", width="100%"),
                    tags$br(),
                    span(("Rate of catch according to the flag of the fishing fleet"),align = "left", style = "font-size:80%"),
                    tags$br(),
                    span(("Circles in the grid shows the detail of this rate for a spefic square of the grid"),align = "left", style = "font-size:80%"),
                    tags$br(),
                    tags$br(),
                    actionButton("refresh_map","Refresh map for this zoom level")
                    ),
      
      absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                    tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))),
      
      absolutePanel(id = "controls", class = "panel panel-default", bottom =  "2%", left = "10%", width = "80%", fixed=TRUE, draggable = FALSE, height = "auto",
                    dygraphOutput("plot1_streamgraph", height="400", width="80%")
      )
  )
  )
}
