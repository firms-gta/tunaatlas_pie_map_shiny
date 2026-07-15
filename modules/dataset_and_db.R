# modules/dataset_and_db.R

dataset_and_db_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::nav_panel(
    title = "Choose dataset and connect",
    value = "dataset_and_db_value",
    
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Data source"),
          shiny::radioButtons(
            ns("source_type"),
            "Choose data source:",
            choices = c("Database" = "db", "Workspace/DOI" = "doi"),
            selected = "doi"
          )
        )
      ),
      
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Options"),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("source_type"), "'] == 'doi'"),
            shiny::uiOutput(ns("doi_panel"))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("source_type"), "'] == 'db'"),
            shiny::uiOutput(ns("db_panel"))
          )
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        bslib::card(
          shiny::actionButton(ns("submitDataset"), "Submit Dataset")
        )
      )
    )
  )
}


dataset_and_db_server <- function(id,
                                  filters_combinations,
                                  default_dataset = NULL,
                                  default_gridtype = NULL,
                                  default_measurement_unit = NULL) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pool_reactive <- shiny::reactiveVal(NULL)
    
    db_results <- shiny::reactiveValues(
      status = "Not connected",
      connected = FALSE
    )
    
    # ----------------------------
    # Helpers
    # ----------------------------
    parse_schema_object <- function(x) {
      parts <- strsplit(x, "\\.")[[1]]
      if (length(parts) != 2) stop("Invalid object name (expected schema.object)")
      list(schema = parts[1], name = parts[2])
    }
    
    close_pool_if_any <- function() {
      old <- pool_reactive()
      if (!is.null(old)) {
        try(pool::poolClose(old), silent = TRUE)
      }
      pool_reactive(NULL)
      db_results$connected <- FALSE
      db_results$status <- "Not connected"
      filters_combinations(NULL)
    }
    
    # Get columns using pg_catalog (works better than information_schema with matviews/permissions)
    get_columns_pg <- function(pool, schema, name) {
      DBI::dbGetQuery(pool, "
        SELECT a.attname AS column_name
        FROM pg_attribute a
        JOIN pg_class c ON c.oid = a.attrelid
        JOIN pg_namespace n ON n.oid = c.relnamespace
        WHERE n.nspname = $1
          AND c.relname = $2
          AND a.attnum > 0
          AND NOT a.attisdropped
        ORDER BY a.attnum;
      ", params = list(schema, name))$column_name
    }
    
    # Find a column among candidates (case-insensitive)
    pick_col <- function(cols, candidates) {
      cols_l <- tolower(cols)
      cand_l <- tolower(candidates)
      idx <- match(cand_l, cols_l)
      idx <- idx[!is.na(idx)]
      if (length(idx) == 0) return(NULL)
      cols[idx[1]]
    }
    
    # ----------------------------
    # DOI panel
    # ----------------------------
    doi_data <- tryCatch(
      readr::read_csv("DOI.csv", show_col_types = FALSE),
      error = function(e) data.frame(DOI = character(), Filename = character())
    )
    
    if (nrow(doi_data) > 0 && all(c("DOI", "Filename") %in% names(doi_data))) {
      for (i in seq_len(nrow(doi_data))) {
        record_id <- sub(".*zenodo\\.([0-9]+)$", "\\1", doi_data$DOI[i])
        filename  <- doi_data$Filename[i]
        dataset   <- tools::file_path_sans_ext(filename)
        ext       <- tools::file_ext(filename)
        doi_data$Filename[i] <- file.path(paste0(dataset, "_", record_id, ".", ext))
      }
    }
    
    output$doi_panel <- shiny::renderUI({
      if (nrow(doi_data) == 0 || !"Filename" %in% names(doi_data)) {
        return(shiny::tags$p("DOI.csv missing or invalid.", style = "color: red;"))
      }
      shiny::selectizeInput(
        ns("select_doi_dataset"),
        "Select Dataset from DOI:",
        choices = doi_data$Filename,
        selected = doi_data$Filename[1]
      )
    })
    
    # ----------------------------
    # DB panel UI
    # ----------------------------
    output$db_panel <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(
          db_results$status,
          style = if (db_results$connected) "color: green;" else "color: #666;"
        ),
        
        shiny::selectInput(
          ns("db_name"),
          "Choose database:",
          choices = c("tunaatlas_sandbox"),
          selected = "tunaatlas_sandbox"
        ),
        
        shiny::selectInput(
          ns("db_object"),
          "Choose DB object:",
          choices = c(
            "Effort (matview: shinyeffort)" = "public.shinyeffort",
            "Catch (matview: shinycatch)" = "public.shinycatch",
            "Catch georef (matview: shinycatch_georef)" = "public.shinycatch_georef"
          ),
          selected = "public.shinyeffort"
        ),
        
        shiny::actionButton(ns("connect_db"), "Connect"),
        shiny::tags$hr(),
        
        shiny::uiOutput(ns("select_dataset")),
        shiny::tags$br(),
        shiny::uiOutput(ns("select_gridtype")),
        shiny::uiOutput(ns("select_measurement_unit"))
      )
    })
    
        # Close pool when leaving DB mode
        shiny::observeEvent(input$source_type, {
          if (input$source_type != "db") close_pool_if_any()
        }, ignoreInit = TRUE)

      # ----------------------------
      # Connect on button
      # ----------------------------
      shiny::observeEvent(input$connect_db, {
        shiny::req(input$source_type == "db")
        shiny::req(input$db_name, input$db_object)

        close_pool_if_any()
        db_results$status <- "Connecting..."

        if (file.exists("connection_tunaatlas_inv.txt")) {
          try(dotenv::load_dot_env("connection_tunaatlas_inv.txt"), silent = TRUE)
        }

        db_host <- Sys.getenv("DB_HOST")
        db_port <- as.integer(Sys.getenv("DB_PORT"))
        db_user_readonly <- Sys.getenv("DB_USER_READONLY")
        db_password <- Sys.getenv("DB_PASSWORD")

        tryCatch({
          pool <- pool::dbPool(
            RPostgreSQL::PostgreSQL(),
            host = db_host,
            port = db_port,
            dbname = input$db_name,
            user = db_user_readonly,
            password = db_password
          )

          if (!pool::dbIsValid(pool)) stop("invalid pool")

          pool_reactive(pool)
          db_results$connected <- TRUE
          db_results$status <- paste("Connected to", input$db_name)

          obj <- parse_schema_object(input$db_object)
          cols <- get_columns_pg(pool, obj$schema, obj$name)

          if (length(cols) == 0) {
            stop("Could not read columns for selected object (permissions?)")
          }

          # Auto-map common column names (adjust candidates if needed)
          dataset_col <- pick_col(cols, c("dataset", "dataset_name", "db_mapping_dataset_name"))
          unit_col    <- pick_col(cols, c("measurement_unit", "unit", "meas_unit"))
          grid_col    <- pick_col(cols, c("gridtype", "grid_type", "grid"))

          # Require at least unit+grid (if your app depends on them)
          if (is.null(unit_col) || is.null(grid_col)) {
            db_results$status <- paste0(
              "Connected, but could not find required columns. Found cols: ",
              paste(cols, collapse = ", ")
            )
            filters_combinations(data.frame())
            return()
          }

          # dataset is optional -> fallback constant
          dataset_expr <- if (!is.null(dataset_col)) {
            glue::glue("{`dataset_col`}")
          } else {
            glue::glue_sql("{shQuote(obj$name)}", .con = pool) # e.g. 'shinyeffort'
          }

          q <- glue::glue_sql("
          SELECT DISTINCT
            {DBI::SQL(dataset_expr)} AS dataset,
            {`unit_col`} AS measurement_unit,
            {`grid_col`} AS gridtype
          FROM {`obj$schema`}.{`obj$name`};
        ", .con = pool)

          filters_data <- DBI::dbGetQuery(pool, q)

          filters_combinations(filters_data)
          db_results$status <- paste(
            "Connected. Loaded", nrow(filters_data), "filter rows from", input$db_object
          )

        }, error = function(e) {
          close_pool_if_any()
          db_results$status <- paste("Connection/query error:", e$message)
        })
      })
      
      # ----------------------------
      # Dataset / gridtype / unit selectors
      # NOTE: single selection by default (avoid length>1 issues elsewhere)
      # ----------------------------
      output$select_dataset <- shiny::renderUI({
        shiny::req(input$source_type == "db")
        dat <- filters_combinations()
        shiny::validate(shiny::need(!is.null(dat), "Not connected. Click Connect."))
        shiny::validate(shiny::need(nrow(dat) > 0, "No filters available for this object."))
        
        datasets <- dat %>% dplyr::distinct(dataset) %>% dplyr::arrange(dataset)
        selected <- if (!is.null(default_dataset) && default_dataset %in% datasets$dataset) {
          default_dataset
        } else {
          datasets$dataset[1]
        }
        
        shiny::selectizeInput(
          ns("select_dataset"),
          "Select the Dataset",
          choices = datasets$dataset,
          selected = selected,
          multiple = FALSE
        )
      })
      
      output$select_gridtype <- shiny::renderUI({
        shiny::req(input$source_type == "db", input$select_dataset)
        dat <- filters_combinations()
        shiny::validate(shiny::need(!is.null(dat) && nrow(dat) > 0, "No filters available."))
        
        gridtypes <- dat %>%
          dplyr::filter(dataset == input$select_dataset) %>%
          dplyr::distinct(gridtype) %>%
          dplyr::arrange(gridtype)
        
        selected <- if (!is.null(default_gridtype) && default_gridtype %in% gridtypes$gridtype) {
          default_gridtype
        } else {
          gridtypes$gridtype[1]
        }
        
        shiny::selectizeInput(
          ns("select_gridtype"),
          "Select the Grid Type",
          choices = gridtypes$gridtype,
          selected = selected,
          multiple = TRUE
        )
      })
      
      output$select_measurement_unit <- shiny::renderUI({
        shiny::req(input$source_type == "db", input$select_dataset)
        dat <- filters_combinations()
        shiny::validate(shiny::need(!is.null(dat) && nrow(dat) > 0, "No filters available."))
        
        units <- dat %>%
          dplyr::filter(dataset == input$select_dataset) %>%
          dplyr::distinct(measurement_unit) %>%
          dplyr::arrange(measurement_unit)
        
        selected <- if (!is.null(default_measurement_unit) && default_measurement_unit %in% units$measurement_unit) {
          default_measurement_unit
        } else {
          units$measurement_unit[1]
        }
        
        shiny::selectizeInput(
          ns("select_measurement_unit"),
          "Select the Measurement Unit",
          choices = units$measurement_unit,
          selected = selected,
          multiple = TRUE
        )
      })
      
      # ----------------------------
      # Return reactives
      # ----------------------------
      list(
        selected_source = shiny::reactive(input$source_type),
        selected_db_object = shiny::reactive(if (input$source_type == "db") input$db_object else NULL),
        
        selected_dataset = shiny::reactive({
          if (input$source_type == "db") input$select_dataset else input$select_doi_dataset
        }),
        
        selected_gridtype = shiny::reactive(input$select_gridtype),
        selected_measurement_unit = shiny::reactive(input$select_measurement_unit),
        
        pool = pool_reactive,
        submit = shiny::reactive(input$submitDataset)
      )
    })
  }