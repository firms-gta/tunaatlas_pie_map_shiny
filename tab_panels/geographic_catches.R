geographic_catches_ui <- function() {
  tabPanel("Interactive Indicator 11",
    filterUI("interactive_tab_species"),
    
    plotOutput(outputId = "plot_species", width="100%")
        ,
        absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed = TRUE, draggable = FALSE, height = "auto",
                      tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg', height='89', width='108')))
    )
  
  
}
