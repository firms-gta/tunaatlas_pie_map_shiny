geographic_catches_by_fishing_fleet_ui <- function() {
  
  nav_panel("Georeferenced catches by fishing fleet",
                      grid_container(
                        layout = c(
                          "pie_map_fishing_fleet  pie_map_fishing_fleet",
                          "plot_fishing_fleet_by_time logo"
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
                          area = "pie_map_fishing_fleet",
                          card_body(leafletOutput("pie_map_fishing_fleet", width="100%", height="100%"))
                        ), 
                        grid_card(area = "plot_fishing_fleet_by_time", 
                                  card_body(dygraphOutput("plot_fishing_fleet_by_time"))
                        )
                      )
            )
           # filterUI("interactive_tab"),
  # div(class="outer",
  #     # tags$head(includeCSS("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/styles.css")),
  #     leafletOutput("map_i11", width="100%", height="100%"),
  #     
  #     
  #     absolutePanel(id = "controls", class = "panel panel-default",
  #                   top = 200, left = "auto", width = "20%", fixed=TRUE,
  #                   draggable = TRUE, height = "auto",
  #                   tags$br(),
  #                   plotOutput("pie_map_i11", width="100%"),
  #                   tags$br(),
  #                   span(("Rate of catch according to the flag of the fishing fleet"),align = "left", style = "font-size:80%"),
  #                   tags$br(),
  #                   span(("Circles in the grid shows the detail of this rate for a spefic square of the grid"),align = "left", style = "font-size:80%"),
  #                   tags$br(),
  #                   tags$br(),
  #                   actionButton("refresh_map","Refresh map for this zoom level")
  #                   ),
  #     
  #     absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
  #                   tags$a(href='https://www.ird.fr/', tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg',height='178',width='216')))
  #     
  # )
  # )
}
