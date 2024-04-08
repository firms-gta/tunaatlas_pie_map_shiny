main_panel_ui = function()
  {
  nav_panel(
    title = "GTA Shiny App", value = "mainpanel",
    card(
      full_screen = TRUE,
      card_header(
        "Indicators for Fisheries: use case of the Tuna Atlas"
      ),
      card_body("The application allows users to explore different indicators related to tuna fisheries. To begin the exploration, users are invited to explore data and select desired filters. Upon submission, the application will render the selected indicators, which may include visual representations such as maps and graphs. The selection of the dataset to explore can be changed in the Dataset selection tab and more info about this app and the global project are displayed in the About panel",
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
            card_header("Data displayed"),
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
                  card_body('plotOutput(outputId = "plot")')
                ),
                grid_card(
                  area = "example_plot",
                  card_body('plotOutput(outputId = "plot")')
                ),
                grid_card(
                  area = "example_grid",
                  card_body('DTOutput(outputId = "myTable", width = "100%")')
                ),
                grid_card_text(
                  content = "Text for card",
                  alignment = "center",
                  area = "example_query"
                )
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

