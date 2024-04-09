#' Get HTML Page Title
#'
#' This function takes the path to an HTML file as input and returns the content of the `<title>` tag.
#' It uses the `rvest` package to read the HTML file and extract the title.
#'
#' @param html_file A string that represents the file path of the HTML file.
#' @return A string containing the text within the `<title>` tag of the HTML document.
#' @examples
#' get_html_title("path/to/your/html_file.html")
#' @importFrom rvest read_html
#' @importFrom xml2 xml_find_first xml_text
#' @export
#'
#' @examples
#' # Assume you have an HTML file named "example.html" in your working directory.
#' # This file has a title tag <title>Example Title</title>
#' get_html_title("example.html")
#' 

get_html_title <- function(html_file) {
  doc <- read_html(html_file)
  title <- xml_find_first(doc, "//title")
  title_text <- xml_text(title)
  return(title_text)
}