ui <- tagList(
  useShinyjs(),
  div(
    id = "loading_page",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: white; z-index: 9999; text-align: center;",
    tags$iframe(
      src = "www/map_init.html",
      style = "width: 100%; height: 100%; border: none;"
    ),
    div(
      style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); background: rgba(255, 255, 255, 0.8); padding: 20px; border-radius: 10px;",
      tags$h1("Welcome to the GlobalTunaAtlas shiny app"),
      tags$p("Please wait for initial data to load")
    )
  ),
  hidden(
    div(id = "main_content",
        page_navbar(
          id = "main",
          title = "Tuna Atlas: Interactive Indicator",
          selected = "datasetchoicevalue",
          collapsible = TRUE,
          theme = bslib::bs_theme(),
          sidebar = sidebar_ui(),
          
            geographic_catches_ui(),
          
          nav_menu(
            title = "Indicators for each variable",
            !!!lapply(dimensions, function(dimension) {
              nav_panel(
                title = dimension$column_name,
                geographic_catches_by_variable_ui(dimension$column_name)
              )
            })
          ),
          
            data_explorer_combined_ui(),
          
          dataset_choice_ui("dataset_choice"),
            main_panel_ui(),
          
            more_about()
          )
        )
    )
  
)
