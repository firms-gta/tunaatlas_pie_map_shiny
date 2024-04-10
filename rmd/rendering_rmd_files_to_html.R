# Render R Markdown documents
rmarkdown::render(here::here("rmd/Application_overview.Rmd"))
rmarkdown::render(here::here("rmd/Datasets.Rmd"))

list_markdown_path <- c("rmd/Authors.html", "rmd/Fundings.html", 
                        "rmd/sidebar_explenations.html", "rmd/General_disclaimer.html", 
                        "rmd/Running_the_app.html")

html_files <- sapply(list_markdown_path, function(html_file) {
  rmd_file <- gsub(".html",".Rmd",html_file)
  # if(!file.exists(html_file)){
  render(rmd_file, output_format = "html_document", quiet = TRUE)
  # }
}, USE.NAMES = FALSE)


