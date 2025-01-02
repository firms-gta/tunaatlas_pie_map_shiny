db_connect_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Database Connection",
    tagList(
      # Choix de la source de données
      uiOutput(ns("choose_data_source")),  # Ajouter le bouton radio pour choisir la source
      
      # Bouton de connexion
      actionButton(
        ns("connect_db"), 
        "Try to connect to GlobalTunaAtlas Database", 
        class = "btn-primary"
      ),
      
      # Statut de connexion
      verbatimTextOutput(ns("connection_status")),
      
      # Tableau des filtres
      tableOutput(ns("filters_table"))
    )
  )
}


# Module Server
db_connect_server <- function(id, filters_combinations) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    db_results <- reactiveValues(
      status = "Waiting for connection to DB..."
    )
    
    # Variable pour stocker le pool de connexion
    pool_reactive <- reactiveVal(NULL)
    
    # Ajouter une sélection pour choisir la source de données
    output$choose_data_source <- renderUI({
      radioButtons(
        ns("data_source"),
        label = "Choose data source:",
        choices = c("Issueddata" = "issueddata", "Shinycatch" = "shinycatch"),
        selected = "issueddata"
      )
    })
    
    # Connexion à la base de données au clic
    observeEvent(input$connect_db, {
      db_results$status <- "Trying to connect..."
      
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
            
            # Requête SQL basée sur la sélection utilisateur pour ne pas tout télécharger si on s'intéresse aux issueddata
            data_source <- input$data_source  
            filters_query <- if (data_source == "issueddata") {
              glue_sql("
                SELECT DISTINCT dataset, measurement_unit, gridtype 
                FROM public.issueddata;", .con = pool)
            } else {
              glue_sql("
                SELECT DISTINCT dataset, measurement_unit, gridtype 
                FROM public.shinycatch;", .con = pool)
            }
            
            filters_data <- DBI::dbGetQuery(pool, filters_query)
            
            if (nrow(filters_data) > 0) {
              filters_combinations(filters_data)
              db_results$status <- "Connection successful! Filters updated."
            } else {
              db_results$status <- "Connection successful but no filters available."
              filters_combinations(data.frame(
                dataset = character(),
                measurement_unit = character(),
                gridtype = character(),
                stringsAsFactors = FALSE
              ))
            }
          } else {
            db_results$status <- "Connection failed: invalid pool."
          }
          
          # Ne fermez pas le pool ici
          # poolClose(pool)
        }, error = function(e) {
          db_results$status <- paste("Connection error:", e$message)
          filters_combinations(data.frame(
            dataset = character(),
            measurement_unit = character(),
            gridtype = character(),
            stringsAsFactors = FALSE
          ))
        })
      } else {
        db_results$status <- "Configuration file missing: 'connection_tunaatlas_inv.txt'."
        filters_combinations(data.frame(
          dataset = character(),
          measurement_unit = character(),
          gridtype = character(),
          stringsAsFactors = FALSE
        ))
      }
    })
    
    # Affichage dans l'UI
    output$connection_status <- renderPrint({ db_results$status })
    output$filters_table <- renderTable({
      req(filters_combinations())
      filters_combinations()
    })
    
    # Retourner le pool pour un usage externe
    return(list(
      pool = pool_reactive
    ))
  })
}



