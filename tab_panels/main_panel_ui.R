main_panel_ui = function()
  {
  nav_panel(
    title = "GTA Shiny App", value = "mainpanel",
    card(
      full_screen = TRUE,
      card_header(
        "Indicators for Fisheries: use case of the Tuna Atlas"
      ),
      card_body(tags$iframe(src = "rmd/Application_overview.html", height = 500, width = "100%"),
        grid_container(
          layout = c(
            "howtouse datadisplayed",
            "howtouse datadisplayed"
          ),
          row_sizes = c(
            "1.73fr",
            "0.27fr"
          ),
          col_sizes = c(
            "0.5fr",
            "1.5fr"
          ),
          gap_size = "10px",
          grid_card(
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
                gap_size = "10px",
                grid_card(
                  area = "map_example",
                  card_body(mapCatchesUI("total_catch_init"))
                ),
                grid_card(
                  area = "example_plot",
                  card_body(plotOutput("plot_init"))
                ),
                grid_card(
                  area = "example_grid",
                  card_body(dataTableOutput("head_table"))
                ),
                grid_card(area = "example_query", textOutput("sql_query_init"))
              )
            )
          ),
          grid_card(
            area = "howtouse",
            full_screen = TRUE,
            card_header("Example of use"),
            card_body(
              grid_container(
                layout = c(
                  "indicator1 indicator1",
                  "button  button"
                ),
                row_sizes = c(
                  "1fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr",
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "indicator1",
                  card_body(
                    "Example of indicators for Albacore tuna in Indian ocean",
                    actionButton(inputId = "buttoni7", label = "Geographes catches"),
                    actionButton(inputId = "buttoni8", label = "Time series by fishing_fleet")
                  )
                ),
                grid_card(
                  area = "button",
                  card_body(
                    "Example of indicators for Skipjack Tuna in 2000",
                    actionButton(inputId = "buttoni7", label = "Geographes catches"),
                    actionButton(inputId = "buttoni8", label = "Geographic catches by fishing_fleet")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

