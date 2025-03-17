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
    require(cowplot)
    require(flextable)
    require(bookdown)
    source(here::here("Markdown/reportmarkdown.R"))
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
        setwd(here::here("Markdown")) # changer le repo au début et pas juste avant de lancer le bookdown sinon shiny est pas assez reactif
        futile.logger::flog.info(paste0("new repository: ", getwd()))
        default_dataset <- dataset_reactive
        default_dataset <- default_dataset %>%
          dplyr::mutate(Time = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>% dplyr::rename(GRIDTYPE = gridtype)
        qs::qsave(default_dataset, "default_dataset.qs")
        child_env_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
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
          parameter_titre_dataset_1 = "Mydataset",
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
        qs::qsave(child_env_last_result, "child_env_last_results.qs")
        # output_dir <- tempdir()
        output_dir <- here::here("www")
        output_file <- file.path(output_dir, "My_report.html")
        bookdown::render_book(
          input = "index.Rmd",
          envir = render_env,
          output_format = "bookdown::html_document2",
          output_file = output_file
        )
        report_file(output_file)
        report_status("Report generated")
        setwd(here::here())
        futile.logger::flog.info(paste0("new repository: ", getwd()))
        
      }, error = function(e) {
        report_status(paste("Error while generating :", e$message))
      })
    })
    
    output$download_link <- renderUI({
      report_path <- report_file()
      
      if (!is.null(report_path) && file.exists(report_path)) {
        # Copier le fichier dans le répertoire www
        www_path <- file.path("www", basename(report_path))
        file.copy(report_path, www_path, overwrite = TRUE)
        
        tags$a(href = paste0("/www/", basename(report_path)), 
               "Downloading the report",
               target = "_blank", 
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
