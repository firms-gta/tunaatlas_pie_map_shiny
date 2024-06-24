main_panel_ui = function()
  {
  nav_panel(title = "GTA Shiny App", value = "mainpanel",
    card(fill = TRUE,
      card_header(
        "Global Tuna Atlas shiny app overview"
      ),
      card_body(fillable = TRUE,
        tags$iframe(src = "www/Application_overview.html", width = "100%"),
        grid_container(
          layout = c(
            "datadisplayed datadisplayed",
            "datadisplayed datadisplayed"
          ),
          row_sizes = c(
            "1.73fr",
            "0.27fr"
          ),
          col_sizes = c(
            "0.5fr",
            "1.5fr"
          ),
          gap_size = "5px",
          grid_card(max_height = 500,
            area = "datadisplayed",
            full_screen = TRUE,
            card_header("Example of displayed data"),
            card_body(
              grid_container(
                layout = c(
                  "map_example  example_plot",
                  "example_grid example_query"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr",
                  "1fr"
                ),
                gap_size = "5px",
                grid_card(max_height = 250,
                  area = "map_example",
                  card_header(strong("Map")),
                  card_body(class = "p-0",tags$iframe(src = "www/map_init.html"))
                ),
                grid_card(card_header(strong("Plot")),max_height = 250,area = "example_plot",card_image(
                  file = here::here("tab_panels/plot_init.png")
                )),
                grid_card(card_header(strong("Datatable")),
                  area = "example_grid",
                  card_body(
                    dataTableOutput("head_table_init"),
                    style = "height: auto; overflow-y: visible;"  # Adjust height and overflow as needed
                  )
                ),
                grid_card(card_header(strong("SQL Queries")),max_height = 250,area = "example_query", textOutput("sql_query_init"),fill = TRUE, height = "100%")
              )
            )
          )
          # grid_card(max_height = 500,
          #   area = "howtouse",
          #   full_screen = TRUE,
          #   card_header("Example of use"),
          #   card_body(
          #     grid_container(
          #       layout = c(
          #         "indicator1 indicator1",
          #         "button  button"
          #       ),
          #       row_sizes = c(
          #         "1fr",
          #         "1fr"
          #       ),
          #       col_sizes = c(
          #         "1fr",
          #         "1fr"
          #       ),
          #       gap_size = "5px",
          #       grid_card(max_height = 250, fill = TRUE,
          #         area = "indicator1",
          #         card_body(fill = TRUE,
          #           "Indicators for major tuna in Indian ocean",
          #           actionButton(inputId = "buttoni7", label = "Geographes catches"),
          #           actionButton(inputId = "buttoni8", label = "Time series by fishing_fleet")
          #         )
          #       ),
          #       grid_card(fill = TRUE,
          #         area = "button",
          #         card_body(max_height = 250, fill = TRUE,
          #           "Indicators for Skipjack Tuna in 2000",
          #           actionButton(inputId = "buttoni7", label = "Geographes catches"),
          #           actionButton(inputId = "buttoni8", label = "Geographic catches by fishing_fleet")
          #         )
          #       )
          #     )
          #   )
          # )
        )
      )
    )
  )
}

