geographic_catches_by_species_ui <- function() {
  nav_panel(title = "Georeferenced catch by species",
            grid_container(
              layout = c(
                "pie_map_species  pie_map_species   ",
                "plot_species_by_time logo"
              ),   
              row_sizes = c(
                "1.48fr",
                "0.52fr"
              ),
              col_sizes = c(
                "1.64fr",
                "0.36fr"
              ),
              gap_size = "10px",
              # grid_card_plot(area = "plot"),
              grid_card_text(
                # Here, use HTML tags directly to embed the image and hyperlink
                content = create_logo_panel(),
                alignment = "start",
                area = "logo"
              ),
              grid_card(
                area = "pie_map_species",
                card_body(leafletOutput("pie_map_species", width="100%", height="100%"))
              ), 
              grid_card(area = "plot_species_by_time", 
               card_body(dygraphOutput("plot_species_by_time"))
            )
  )
  )
#   nav_panel(title = "Interactive Indicator 11 for species",
#     leafletOutput("pie_map_species", width="100%", height="100%"),
#     
#     absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
#                   tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216'))),
#     
#     absolutePanel(id = "controls", class = "panel panel-default", bottom =  "2%", left = "10%", width = "80%", fixed=TRUE, draggable = FALSE, height = "auto",
#                   dygraphOutput("plot_species_by_time", height="400", width="80%"))
# )
# )
}
