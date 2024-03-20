interactive_indicator_11_ui <- function() {
  tabPanel("Interactive Indicator 11",
  div(class="outer",
      tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
      # leafletOutput('map_i11', width = "60%", height = 1500),
      leafletOutput("map_i11", width="100%", height="100%"),
      
      
      absolutePanel(id = "controls", class = "panel panel-default",
                    top = 200, left = "auto", width = "20%", fixed=TRUE,
                    draggable = TRUE, height = "auto",
                    
                    # h3(textOutput("sql_query"), align = "right"),
                    # plotOutput("plot1_streamgraph", height=200, width="100%"),
                    # dygraphOutput("plot1_streamgraph", height=400, width="100%"),
                    tags$br(),
                    plotOutput("pie_map_i11", width="100%"),
                    tags$br(),
                    span(("Rate of catch according to the flag of the fishing fleet"),align = "left", style = "font-size:80%"),
                    tags$br(),
                    span(("Circles in the grid shows the detail of this rate for a spefic square of the grid"),align = "left", style = "font-size:80%"),
                    tags$br(),
                    tags$br(),
                    actionButton("refresh_map","Refresh map for this zoom level")
                    
                    # sliderTextInput("sars_plot_date",
                    #                 label = h5("Select mapping date"),
                    #                 choices = format(unique(sars_cases$date), "%d %b %y"),
                    #                 # selected = format(sars_max_date, "%d %b %y"),
                    #                 grid = FALSE,
                    #                 animate=animationOptions(interval = 3000, loop = FALSE))
      ),
      
      absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                    tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))),
      
      absolutePanel(id = "controls", class = "panel panel-default", bottom =  "2%", left = "10%", width = "80%", fixed=TRUE, draggable = FALSE, height = "auto",
                    dygraphOutput("plot1_streamgraph", height="400", width="80%")
      )
  )
  )
}