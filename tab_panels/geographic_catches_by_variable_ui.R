geographic_catches_by_variable_ui <- function(variable) {
  nav_panel(title = paste0("Indicators by ",  variable),
            grid_container(
              layout = c(
                "slider slider slider",
                "map map map",
                "plot    plot   pie_map"
              ),
              row_sizes = c("0.1fr","1.1", "0.7fr"),
              col_sizes = c("1fr", "1fr", "1fr"),
              gap_size = "5px",   
              grid_card_text(
                content = shinycssloaders::withSpinner(uiOutput(paste0("slider_ui_", variable))),
                area = "slider"),
              grid_card(
                area = "map",
                card_body(pieMapTimeSeriesUI(paste0(variable,"_module")))
              ),
              grid_card(
                area = "plot",
                card_body(TimeSeriesbyDimensionUI(paste0(variable,"_timeseries")))
              ), 
              grid_card(
                area = "pie_map", card_body(categoryGlobalPieChartUI(paste0(variable,"_chart"))))
    )
  )
}






