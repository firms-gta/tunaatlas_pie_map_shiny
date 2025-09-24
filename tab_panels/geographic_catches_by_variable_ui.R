geographic_catches_by_variable <- function() {
  tagList(
    # Sélecteur de dimension (rempli côté server via output$dim_selector)
    div(style = "margin-bottom: .75rem;", uiOutput("dim_selector")),
    # Slider top-N (rempli côté server via output$slider_ui)
    div(style = "margin-bottom: .75rem;", uiOutput("slider_ui")),
    
    grid_container(
      layout    = c("plot", "map"),
      row_sizes = c("1fr", "1fr"),
      col_sizes = "1fr",
      gap_size  = "6px",
      
      grid_card(
        area = "plot",
        card_body(
          # ID FIXE pour les charts car plus besoin vu que c'est plus un lapply, c'était intéressant, à garder en mémoire mais trop complexe pour la réactivité
          categoryGlobalChartUI("chart")
        )
      ),
      
      grid_card(
        area = "map",
        card_body(
          # ID FIXE pour la carte / time series
          pieMapTimeSeriesUI("map")
        )
      )
    )
  )
}

