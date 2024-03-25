geographic_catches_ui <- function() {
nav_panel(title = "Georeferenced catch total",
            grid_container(
              layout = c(
                "total_catch  total_catch   ",
                "irdetc irdetc"
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
                area = "irdetc"
              ),
              grid_card(
                area = "total_catch",
                card_body(leafletOutput("total_catch", width="100%", height="100%"))
              )
    )
  )
  
  
}
