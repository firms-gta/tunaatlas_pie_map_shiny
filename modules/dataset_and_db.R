dataset_and_db_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Choose dataset and connect",
    value = "dataset_and_db_value",
    
    grid_container(
      layout = c(
        "choosesourcetype subpanels",
        "submitbutton submitbutton"
      ),
      row_sizes = c(
        "400px",
        "400px"
      ),
      col_sizes = c(
        "0.5fr", 
        "0.5fr"
      ),
      
      # Choix de la source de données
      grid_card(
        area = "choosesourcetype",
        card_body(
          radioButtons(ns("source_type"), "Choose data source:", 
                       choices = c("Database" = "db", "Workspace/DOI" = "doi"), 
                       selected = "doi")
        )
      ),
      
      # Contenu conditionnel : DOI ou Database
      grid_card(
        area = "subpanels",
        card_body(
          conditionalPanel(
            condition = paste0("input['", ns("source_type"), "'] == 'doi'"),
            uiOutput(ns("doi_panel"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("source_type"), "'] == 'db'"),
            uiOutput(ns("db_panel"))
          )
        )
      ),
      
      # Bouton de soumission
      grid_card(
        area = "submitbutton",
        card_body(actionButton(ns("submitDataset"), "Submit Dataset"))
      )
    )
  )
}


dataset_and_db_server <- function(id, filters_combinations, default_dataset, default_gridtype, default_measurement_unit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pool_reactive <- reactiveVal(NULL)
    # ReactiveValues pour stocker le statut de la connexion DB
    db_results <- reactiveValues(
      status = "Not connected",
      pool = NULL
    )
    
    # Charger les données DOI une fois
    doi_data <- tryCatch(
      read_csv("DOI.csv", show_col_types = FALSE),
      error = function(e) {
        flog.error("Erreur lors de la lecture du fichier DOI.csv : %s", e$message)
        data.frame(Filename = character())
      }
    )
    
    if (nrow(doi_data) == 0 || !"Filename" %in% colnames(doi_data)) {
      stop("Le fichier DOI.csv est invalide ou vide.")
    }
    
    # Panel DOI
    output$doi_panel <- renderUI({
      tagList(
        selectizeInput(
          ns("select_doi_dataset"),
          "Select Dataset from DOI:",
          choices = doi_data$Filename,
          selected = doi_data$Filename[1]
        )
      )
    })
    
    # Connexion à la base de données
    observeEvent(input$source_type, {
      if (input$source_type == "db") {
        db_results$status <- "Connecting to the database..."
        if (file.exists("connection_tunaatlas_inv.txt")) {
          try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"))
          
          db_host <- Sys.getenv("DB_HOST")
          db_port <- as.integer(Sys.getenv("DB_PORT"))
          db_name <- Sys.getenv("DB_NAME")
          db_user_readonly <- Sys.getenv("DB_USER_READONLY")
          db_password <- Sys.getenv("DB_PASSWORD")
          
          tryCatch({
            pool <- dbPool(RPostgreSQL::PostgreSQL(),
                           host = db_host,
                           port = db_port,
                           dbname = db_name,
                           user = db_user_readonly,
                           password = db_password)
            
            if (pool::dbIsValid(pool)) {
              flog.info("Connexion valide.")
              pool_reactive(pool)
              
              # Récupérer les filtres de la base de données
              filters_query <- glue_sql("
                SELECT DISTINCT dataset, measurement_unit, gridtype 
                FROM public.issueddata;", .con = pool)
              filters_data <- DBI::dbGetQuery(pool, filters_query)
              
              filters_combinations(filters_data)
              db_results$status <- "Connected successfully!"
            } else {
              db_results$status <- "Connection failed: invalid pool."
            }
          }, error = function(e) {
            db_results$status <- paste("Connection error:", e$message)
          })
        } else {
          db_results$status <- "Configuration file missing: 'connection_tunaatlas_inv.txt'."
        }
      }
    })
    
    # Panel Database
    output$db_panel <- renderUI({
      if (is.null(filters_combinations()) || nrow(filters_combinations()) == 0) {
        return(tags$p(db_results$status, style = "color: red;"))
      }
      tagList(
        uiOutput(ns("select_dataset")),
        tags$br(),
        uiOutput(ns("select_gridtype")),
        uiOutput(ns("select_measurement_unit"))
      )
    })
    
    # UI pour sélectionner un dataset
    output$select_dataset <- renderUI({
      req(filters_combinations())
      datasets <- filters_combinations() %>% dplyr::select(dataset) %>% dplyr::distinct()
      selectizeInput(
        ns("select_dataset"),
        "Select the Dataset",
        choices = datasets$dataset,
        selected = ifelse(nrow(datasets) > 0, default_dataset, NULL)
      )
    })
    
    # UI pour sélectionner un grid type
    output$select_gridtype <- renderUI({
      req(filters_combinations(), input$select_dataset)
      gridtypes <- filters_combinations() %>%
        dplyr::filter(dataset == input$select_dataset) %>%
        dplyr::select(gridtype) %>%
        dplyr::distinct()
      selectizeInput(
        ns("select_gridtype"),
        "Select the Grid Type",
        choices = gridtypes$gridtype,
        selected = ifelse(nrow(gridtypes) > 0, gridtypes$gridtype, NULL),
        multiple = TRUE
      )
    })
    
    # UI pour sélectionner une unité de mesure
    output$select_measurement_unit <- renderUI({
      req(filters_combinations(), input$select_dataset)
      measurement_units <- filters_combinations() %>%
        dplyr::filter(dataset == input$select_dataset) %>%
        dplyr::select(measurement_unit) %>%
        dplyr::distinct()
      selectizeInput(
        ns("select_measurement_unit"),
        "Select the Measurement Unit",
        choices = measurement_units$measurement_unit,
        selected = ifelse(nrow(measurement_units) > 0, measurement_units$measurement_unit, NULL),
        multiple = TRUE
      )
    })
    
    # Retourner les valeurs sélectionnées et la connexion
    return(list(
      selected_source = reactive(input$source_type),
      selected_dataset = reactive({
        if (input$source_type == "db") {
          input$select_dataset
        } else {
          input$select_doi_dataset
        }
      }),
      selected_gridtype = reactive(input$select_gridtype),
      selected_measurement_unit = reactive(input$select_measurement_unit),
      pool = pool_reactive,
      submit = reactive(input$submitDataset)
    ))
  })
}


