ui <- fluidPage(
  titlePanel("Tuna Atlas: indicateurs cartographiques i11"),
  navbarPage(title="TunaAtlas", 
             interactive_ui(),
             interactive_indicator_11_ui(),
             interactive_indicator_11_species_ui(),
             ggplot_indicator_11_ui(),
             zoom_level_ui(),
             data_explorer_overview_ui(),
             additional_info_ui()
  )
)