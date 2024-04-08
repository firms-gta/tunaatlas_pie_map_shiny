# Render R Markdown documents
rmd_files <- list.files(here::here("rmd"), full.names = TRUE, pattern = "\\.rmd$")

html_files <- sapply(rmd_files, function(rmd) {
  render(rmd, output_format = "html_document", quiet = TRUE)
}, USE.NAMES = FALSE)

documentTabs <- lapply(seq_along(html_files), function(i) {
  tabPanel(title = titles[i], 
           HTML(paste(readLines(html_files[i]), collapse = "\n")))
})
