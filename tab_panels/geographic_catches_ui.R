geographic_catches_ui <- function() {
  nav_panel(
    title = "General overview",
        grid_container(
          layout = c(
            "mapcatches   by_month  ",
            "plot_catches by_month"
          ),
          row_sizes = c("1.48fr", "0.52fr"),
          col_sizes = c("1.4fr", "0.6fr"),
          gap_size = "10px",
          grid_card(
            area = "mapcatches",
            card_body(mapCatchesUI("total_catch")%>% withSpinner())
          ),
          grid_card(area = "plot_catches", card_body(dygraphOutput("plot_by_time")%>% withSpinner())),             
          grid_card(area = "by_month", card_body(catches_by_variable_moduleUI("catches_by_variable_month")))

        )
      )
}
