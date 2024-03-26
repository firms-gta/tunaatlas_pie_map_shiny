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
          row_sizes = c("1fr", "1fr"),
          col_sizes = c("1.1fr", "0.9fr"),
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
          card_header("Header"),
          card_body(
            grid_container(
              layout = c(
                "by_month phrase_output",
                "by_month  phrase_output"
              ),
              row_sizes = c("1fr", "1fr"),
              col_sizes = c("1.52fr", "0.48fr"),
              gap_size = "10px",
              grid_card(
                area = "phrase_output",
                card_body("testt")#textOutput(outputId = "textOutput"))
              ),
              grid_card(area = "by_month", card_body(catches_by_variable_moduleUI("catches_by_variable_month")))#,
              # grid_card_plot(area = "by_year", card_body(catches_by_variable_moduleUI("catches_by_variable_year")))
            )
          )
        )
      )
    )
  )
}
