ui <- tagList(
  useShinyjs(),
  div(
    id = "loading_page",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: white; z-index: 9999; text-align: center;",
    
    # IFrame pour le chargement
    tags$iframe(
      src = "www/map_init.html",
      style = "width: 100%; height: 100%; border: none;"
    ), 
    div(
      style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); background: rgba(255, 255, 255, 0.8); padding: 20px; border-radius: 10px; text-align: center;",
      
      tags$h1("Welcome to the GlobalTunaAtlas shiny app"),
      
      tags$p("Please wait for data to load")
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
          # sidebar = uiOutput("sidebar_ui_with_variable_to_display"),
          sidebar = nav_panel("sidebarfilter", uiOutput("sidebar_ui_with_variable_to_display")),
          # sidebar = sidebar_ui(), # to make work the toggle little fleche
          geographic_catches_ui(),
          nav_panel(
            title = "Other dimensions",
            uiOutput("dynamic_panels")  # Dynamic nav panels within a nav_menu
          ),
          tabPanel("CSV-based Filtering",
                   sidebarLayout(
                     sidebarPanel(
                       fileInput("file_upload", "Upload CSV", accept = ".csv"),
                       actionButton("apply_csv_filters", "Apply CSV Filters", class = "btn-success"),
                       actionButton("reset_csv_filters", "Reset Filters", class = "btn-warning")
                     ),
                     mainPanel(
                       DTOutput("filtered_data_table")
                     )
                   )
          ),
          data_explorer_combined_ui(),
          dataset_choice_ui("dataset_choice"),
          main_panel_ui(),
          
            more_about()
          )
        )
    )
  
)

