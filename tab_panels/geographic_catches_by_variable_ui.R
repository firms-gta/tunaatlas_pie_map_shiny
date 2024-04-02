geographic_catches_by_variable_ui <- function(variable) {
  nav_panel(title = paste0("Georeferenced catch by ",  variable),
            grid_container(
              layout = c(
                "map_pie_map_and_plot  pie_map",
                "map_pie_map_and_plot logo"
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
                area = "map_pie_map_and_plot",
                card_body(pieMapTimeSeriesUI(paste0(variable,"_module")))
              ), 
              grid_card(area = "pie_map", card_body(categoryGlobalPieChartUI(paste0(variable,"_chart"))))
  )
  )
}

