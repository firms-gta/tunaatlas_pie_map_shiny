# report_module.R

# Module UI
reportModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("generate_report"), "Generate report"),
    uiOutput(ns("report_links")), 
    textOutput(ns("report_status"))
  )
}

# Module Server
reportModuleServer <- function(id, dataset_reactive, rmd_path) {
  moduleServer(id, function(input, output, session) {
    # source(here::here("Markdown/reportmarkdown.R"))
    ns <- session$ns
    
    # Réactive pour stocker le statut du rapport
    report_status <- reactiveVal("Please wait")
    
    # Réactive pour stocker le chemin du fichier généré
    report_file <- reactiveVal()
    
    # Observer pour générer le rapport
    observeEvent(input$generate_report, {
      report_status("Generating report...")
      require(CWP.dataset)
      if(!exists("shp_raw")){
      cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
      shp_raw <- st_as_sf(sf::st_read(cwp_grid_file, show_col_types = FALSE) %>% dplyr::rename(geom = geom_wkt), wkt = "geom")
      }
      if(!exists("continent")){
        
      WFS <- ows4R::WFSClient$new(
        url = "https://www.fao.org/fishery/geoserver/fifao/wfs",
        serviceVersion = "1.0.0",
        logger = "INFO"
      )
      continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
      sf::st_crs(continent) <- 4326
      }
      tryCatch({
        # Récupération des données réactives
        setwd(here::here("Markdown")) # changer le repo au début et pas juste avant de lancer le bookdown sinon shiny est pas assez reactif
        futile.logger::flog.info(paste0("new repository: ", getwd()))
        default_dataset <- dataset_reactive
        default_dataset <- default_dataset %>%
          dplyr::mutate(Time = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
        child_env_last_result <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
          parameter_init = default_dataset,
          parameter_final = NULL,
          parameter_time_dimension = "Time",
          plotting_type = "view",
          fig.path = NULL,
          parameter_fact = "catch",
          parameter_geographical_dimension_groupping = "gridtype",
          parameter_colnames_to_keep = setdiff(c(variable_to_display, "measurement_value"), c("gridtype", "gear_type", "species", "fishing_fleet")),
          coverage = TRUE,
          shapefile_fix = shp_raw,
          continent = continent,
          parameter_resolution_filter = NULL,
          parameter_titre_dataset_1 = "Mydataset",
          unique_analyse = TRUE, print_map = FALSE
        )
        
        # Configuration des paramètres du rapport
        child_env_last_result$title_markdown <- "Customized report for specific data from Global Tuna Atlas"
        child_env_last_result$fig.path <- "Figures"
        child_env_last_result$Add_lines <- "Add_lines.Rmd"
        child_env_last_result$step_title_t_f <- FALSE
        child_env_last_result$parameter_short <- FALSE
        child_env_last_result$explenation <- FALSE
        child_env_last_result$coverage <- TRUE 
        child_env_last_result$treatment <- FALSE
        child_env_last_result$parameter_mapped <- TRUE
        child_env_last_result$unique_analyse <- TRUE
        child_env_last_result$parameter_titre_dataset_1 <- "My dataset"
        child_env_last_result$child_header <- "#"
        
        render_env <- new.env()
        list2env(child_env_last_result, render_env)
        # output_dir <- tempdir()
        output_dir <- here::here()
        output_file <- file.path(output_dir, "My_report.html")
        
        # Supprimer toute version précédente
        if (file.exists(output_file)) {
          unlink(output_file)
        }
        require(bookdown)
        base::options(knitr.duplicate.label = "allow")
        # Génération du rapport avec Bookdown
        rmarkdown::render(system.file("rmd/comparison.Rmd", package = "CWP.dataset"),
                          envir = render_env,
                          output_file = output_file,
                          output_format = "html_document2")
         
        # Vérification de l'existence du fichier
        if (file.exists(output_file)) {
          report_file(output_file)
          report_status("Report generated successfully.")
        } else {
          report_status("Report generation failed: File not found")
        }
        
        # Retour au répertoire initial
        setwd(here::here())
        futile.logger::flog.info(paste0("Back to repository: ", getwd()))
        
      }, error = function(e) {
        report_status(paste("Error while generating:", e$message))
      })
    })
    
    # Télécharger le rapport
    output$download_report <- downloadHandler(
      filename = function() {
        "My_report.html"
      },
      content = function(file) {
        file.copy(report_file(), file)
      },
      contentType = "text/html"
    )
    
    # Interface de prévisualisation et téléchargement
    output$report_links <- renderUI({
      if (!is.null(report_file()) && file.exists(report_file())) {
        tagList(
          tags$p("Preview the report:", tags$a(href = "My_report.html", "Click here", target = "_blank")),
          downloadButton(ns("download_report"), "Download the report", class = "btn btn-primary")
        )
      } else {
        "No report available"
      }
    })
    
    # Afficher le statut
    output$report_status <- renderText({
      report_status()
    })
  })
}
