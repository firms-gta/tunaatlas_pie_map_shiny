# report_module.R

# Module UI
reportModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("generate_report"), "Generate report"),
    uiOutput(ns("download_link")), 
    textOutput(ns("report_status")) 
  )
}

# Module Server
reportModuleServer <- function(id, dataset_reactive, rmd_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Réactive pour stocker le statut
    report_status <- reactiveVal("En attente...")
    
    # Réactive pour stocker le chemin du rapport généré
    report_file <- reactiveVal()
    
    # Observer pour générer le rapport
    observeEvent(input$generate_report, {
      report_status("Génération en cours...")
      tryCatch({
        # Récupération des données réactives
        default_dataset <- dataset_reactive
        default_dataset <-   default_dataset %>%
          dplyr::mutate(
            Time = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")) # Combine year and month
          ) %>% dplyr::rename(GRIDTYPE = gridtype)
        
        child_env_last_result <- comprehensive_cwp_dataframe_analysis(
          parameter_init = default_dataset,
          parameter_final = NULL,
          parameter_time_dimension = "Time",
          fig.path = NULL,
          parameter_fact = "catch",
          parameter_geographical_dimension_groupping = "GRIDTYPE",
          parameter_colnames_to_keep = setdiff(c(variable_to_display, "measurement_value"), c("GRIDTYPE", "gear_type", "species")),
          coverage = TRUE,
          shapefile_fix = shapefile.fix %>% dplyr::rename(code = geographic_identifier, geom = geom_wkt, GRIDTYPE = gridtype),
          continent = NULL,
          parameter_resolution_filter = NULL,
          parameter_titre_dataset_1 = "My_dataset",
          unique_analyse = TRUE
        )
        
        
        child_env_last_result$step_title_t_f <- FALSE
        child_env_last_result$parameter_short <- FALSE
        child_env_last_result$explenation <- FALSE
        child_env_last_result$coverage <- TRUE
        child_env_last_result$treatment <- FALSE
        child_env_last_result$parameter_mapped <- TRUE
        child_env_last_result$unique_analyse <- TRUE
        child_env_last_result$parameter_titre_dataset_1 <- "My dataset"
        child_env_last_result$child_header <- "#"
        child_env_last_result$title_markdown <- "Customized report for specific data from Global Tuna Atlas"
        child_env_last_result$fig.path <- "Figures"
        child_env_last_result$Add_lines <- "Add_lines.Rmd"
        
        render_env <- new.env()
        list2env(child_env_last_result, render_env)
        setwd(here::here("Markdown")) 
        
        # Dossier temporaire
        output_dir <- tempdir()
        output_file <- file.path(output_dir, "My_report.pdf")
        
        # Générer le rapport
        bookdown::render_book(
          input = "index.Rmd",
          envir = render_env,
          output_format = "bookdown::pdf_document2",
          output_file = output_file
        )
        setwd(here::here())
        # Mettre à jour les réactifs
        report_file(output_file)
        report_status("Report generated")
      }, error = function(e) {
        report_status(paste("Error while generating :", e$message))
      })
    })
    
    output$download_link <- renderUI({
      if (!is.null(report_file())) {
        tags$a(href = paste0("/reports/", basename(report_file())), 
               "Downloading the report",
               target = "_blank", # Ouvre le lien dans un nouvel onglet
               class = "btn btn-primary")
      } else {
        "No report for now"
      }
    })
    
    
    # Afficher le statut
    output$report_status <- renderText({
      report_status()
    })
  })
}
