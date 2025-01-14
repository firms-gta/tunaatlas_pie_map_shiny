output_file_name <- paste0("Dataset_report.html")
render_env <- new.env()
setwd(here::here())

new_repo_path = "Markdown"
files_to_copy <- c("~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Main_caracteristics_of_dataset.Rmd",
                   "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/comparison.Rmd", 
                   "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Explenation.Rmd", 
                   "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/Setup_markdown.Rmd", 
                   "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/template.tex")
for (file in files_to_copy) {
  new_file_path <- file.path(new_repo_path, basename(file))
  file.copy(file, new_file_path, overwrite = FALSE)
  message(paste0("Copy of ", file, " in ", new_file_path))
}
# copy_project_files(original_repo_path = "~/firms-gta/geoflow-tunaatlas/Analysis_markdown", new_repo_path = "Markdown", )

file_path_url <- "~/firms-gta/geoflow-tunaatlas/Analysis_markdown/functions"
source(file.path(file_path_url,"copy_project_files.R"), local = TRUE)
source(file.path(file_path_url,"tidying_GTA_data_for_comparison.R"))
source(file.path(file_path_url,"Functions_markdown.R"), local = TRUE)
source(file.path(file_path_url,"compare_temporal_differences_dygraphs.R"), local = TRUE)
source(file.path(file_path_url,"other_dimension_analysis_dygraphs.R"), local = TRUE)
source(file.path(file_path_url,"Groupping_differences.R"), local = TRUE)
source(file.path(file_path_url,"compare_strata_differences.R"), local = TRUE)
source(file.path(file_path_url,"compare_dimension_differences.R"), local = TRUE)
source(file.path(file_path_url,"compare_temporal_differences.R"), local = TRUE)
source(file.path(file_path_url,"geographic_diff.R"), local = TRUE)
source(file.path(file_path_url,"time_coverage_analysis.R"), local = TRUE)
source(file.path(file_path_url,"spatial_coverage_analysis.R"), local = TRUE)
source(file.path(file_path_url,"other_dimension_analysis.R"), local = TRUE)
source(file.path(file_path_url,"comprehensive_cwp_dataframe_analysis.R"), local = TRUE)
source(file.path(file_path_url,"process_fisheries_data.R"), local = TRUE)
require(cowplot)
# default_dataset <-   default_dataset %>%
#   dplyr::mutate(
#     Time = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")) # Combine year and month
#   ) %>% dplyr::rename(GRIDTYPE = gridtype)
# 
# child_env_last_result <- comprehensive_cwp_dataframe_analysis(
#   parameter_init = default_dataset,
#   parameter_final = NULL,
#   parameter_time_dimension = "Time",
#   fig.path = NULL,
#   parameter_fact = "catch",
#   parameter_geographical_dimension_groupping = "GRIDTYPE",
#   parameter_colnames_to_keep = setdiff(c(variable_to_display, "measurement_value"), c("GRIDTYPE", "gear_type", "species")),
#   coverage = TRUE,
#   shapefile_fix = shapefile.fix %>% dplyr::rename(code = geographic_identifier, geom = geom_wkt, GRIDTYPE = gridtype),
#   continent = NULL,
#   parameter_resolution_filter = NULL,
#   parameter_titre_dataset_1 = "My_dataset",
#   unique_analyse = TRUE
# )
# 
# 
# child_env_last_result$step_title_t_f <- FALSE
# child_env_last_result$parameter_short <- FALSE
# child_env_last_result$explenation <- FALSE
# child_env_last_result$treatment <- FALSE
# child_env_last_result$parameter_mapped <- TRUE
# child_env_last_result$unique_analyse <- TRUE
# child_env_last_result$parameter_titre_dataset_1 <- "My dataset"
# child_env_last_result$child_header <- "#"
# child_env_last_result$title_markdown <- "Customized report for specific data from Global Tuna Atlas"
# child_env_last_result$fig.path <- "Figures"
# child_env_last_result$Add_lines <- "Add_lines.Rmd"
# 
# 
require(flextable)
# render_env <- new.env()
# list2env(child_env_last_result, render_env)
# setwd(here::here("Markdown"))
base::options(knitr.duplicate.label = "allow")
require(bookdown)
# bookdown::render_book("index.Rmd", envir = render_env, output_format = "bookdown::pdf_document2", output_file = "My_report.pdf")
# bookdown::render_book("index.Rmd", envir = render_env, output_format = "bookdown::markdown_document2", output_file = "My_report.md")
# bookdown::render_book("index.Rmd", envir = render_env, output_format = "bookdown::html_document2", output_file = "My_report.html")
