interactive_indicator_11_species_ui <- function() {
  tabPanel("Interactive Indicator 11 for species",
div(class="outer",
    tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
    leafletOutput("pie_map_species", width="100%", height="100%"),
    
    absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                  tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216')))
)
)
}
