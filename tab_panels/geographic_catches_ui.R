geographic_catches_ui <- function() {
  
  nav_panel(
    title = "General overview",
    tabsetPanel(
      nav_panel(
        title = "General catches",
        grid_container(
          layout = c(
            "mapcatches   mapcatches  ",
            "plot_catches plot_catches"
          ),
          row_sizes = c(
            "1fr",
            "1fr"
          ),
          col_sizes = c(
            "1.1fr",
            "0.9fr"
          ),
          gap_size = "10px",
          grid_card(
            area = "mapcatches",
            card_body(mapCatchesUI("total_catch"))
          ),
          grid_card(area = "plot_catches", card_body(dygraphOutput("plot_by_time")))
        )
      ),
      nav_panel(
        title = "General catches by variable",
        card(
          full_screen = TRUE,
          card_header(
            "Header",
            selectInput(
              inputId = "mySelectInput",
              label = "Variable to display",
              choices = list("choice a" = "a", "choice b" = "b"),
              width = "20px",
              selected = "a"
            )
          ),
          card_body(
            sliderInput(
              inputId = "inputId",
              label = "Top number of : variable to display ",
              min = 0,
              max = 10,
              value = 5,
              width = "100%"
            ),
            radioButtons(
              inputId = "myRadioButtons",
              label = "Checkbox Group",
              choices = list("Discrete value" = "a", "Continuous value" = "b"),
              width = "100%"
            ),
            grid_container(
              layout = c(
                "by_month phrase__output",
                "by_year  phrase__output"
              ),
              row_sizes = c(
                "1fr",
                "1fr"
              ),
              col_sizes = c(
                "1.52fr",
                "0.48fr"
              ),
              gap_size = "10px",
              grid_card(
                area = "phrase__output",
                card_body(textOutput(outputId = "textOutput"))
              ),
              grid_card_plot(area = "by_month"),
              grid_card_plot(area = "by_year")
            )
          )
        )
      )
    )
  )
# nav_panel(title = "Georeferenced catch total",
#             grid_container(
#               layout = c(
#                 "total_catch  total_catch   ",
#                 "plot_total_catch irdetc"
#               ),   
#               row_sizes = c(
#                 "1.48fr",
#                 "0.52fr"
#               ),
#               col_sizes = c(
#                 "1.64fr",
#                 "0.36fr"
#               ),
#               gap_size = "10px",
#               # grid_card_plot(area = "plot"),
#               grid_card_text(
#                 # Here, use HTML tags directly to embed the image and hyperlink
#                 content = create_logo_panel(),
#                 alignment = "start",
#                 area = "irdetc"
#               ),
#               grid_card(
#                 area = "total_catch",
#                 card_body(mapCatchesUI("total_catch"))
#               ),
#               grid_card(
#                 area = "plot_total_catch",
#                 card_body(catches_by_variable_moduleUI("plot_by_variable"))
#               )
#     )
#   )
  
  
}
