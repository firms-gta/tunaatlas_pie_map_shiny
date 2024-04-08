RMarkdownOutputsui <- function(html_files) {
  get_html_title <- function(html_file) {
    doc <- read_html(html_file)
    title <- xml_find_first(doc, "//title")
    title_text <- xml_text(title)
    return(title_text)
  }
  
  navbarMenu(title = "R Markdown Documents", id = "rmd_docs",
             lapply(seq_along(html_files), function(i) {
               tabPanel(
                 title = get_html_title(html_files[i]),
                 includeHTML(html_files[i])
               )
             })
  )
}

# 
# RMarkdownOutputsui <- function(html_files) {
#   get_html_title <- function(html_file) {
#     doc <- read_html(html_file)
#     title <- xml_find_first(doc, "//title")
#     title_text <- xml_text(title)
#     return(title_text)
#   }
#   
#   titles <- lapply(html_files, get_html_title)
#   
#   tabPanels <- lapply(seq_along(html_files), function(i) {
#     tabPanel(
#       title = titles[i][[1]],
#       htmltools::includeHTML(html_files[i])
#     )
#   })
#   
#   navbarMenu(
#     title = "R Markdown Documents",
#     id = "rmd_docs",
#     lapply(seq_along(html_files), function(i) {
#       tabPanel(
#         title = get_html_title(html_files[i]),
#         htmltools::includeHTML(html_files[i])
#       )
#     })
#   )
# }
