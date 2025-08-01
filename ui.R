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
      
      tags$p("Please wait for data to load"),
      
      tags$h2(id = "countdown", "10")  # Affiche le compteur de 10 secondes
    ),
    
    # JavaScript pour le décompte (on ne masque par la page car géré dans server.R)
    tags$script(HTML("
    var countdown = 10;
    function updateCountdown() {
      document.getElementById('countdown').innerText = countdown;
      if (countdown > 0) {
        countdown--;
        setTimeout(updateCountdown, 1200);
      }
    }
    setTimeout(updateCountdown, 1200);
"))
  ),
  hidden(
    div(id = "main_content",
        page_navbar(
          id = "main",
          title = "Tuna Atlas: Interactive Indicator",
          selected = "generaloverview",
          collapsible = FALSE,
          theme = bslib::bs_theme(),
          fillable = TRUE,
          gap      = "0.5rem",
          padding     = c(0,0), 
          # sidebar = uiOutput("sidebar_ui_with_variable_to_display"),
          sidebar = nav_panel("sidebarfilter", uiOutput("sidebar_ui_with_variable_to_display")),
          # sidebar = sidebar_ui(), # to make work the toggle little fleche
          geographic_catches_ui(),
          nav_panel(
            title = "Other dimensions",
            uiOutput("dynamic_panels") ,
            padding     = c(0,0), fillable = TRUE, collapsible = FALSE, gap      = "0.5rem",footer = NULL, fluid = FALSE
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
          # db_connect_ui("db_module"),
          # dataset_choice_ui("dataset_choice"),
          dataset_and_db_ui("dataset_and_db_module"),
          outputmoreabout,
          main_panel_ui(), 
          nav_panel(
            title = "Report generation",
            # on crée un conteneur grid 2 colonnes, écart 1rem
            div(
              style = "display: grid;
               grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
               grid-gap: 1rem;",
              reportModuleUI("report_module_1")
              # + d'autres modules ou widgets si vous voulez
            )
          ),
          )
        )
    )
  
)

