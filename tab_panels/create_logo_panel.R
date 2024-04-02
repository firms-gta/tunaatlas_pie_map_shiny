#' Create Logo Panel
#'
#' This function generates a div containing an anchor (\code{<a>}) element that links to the IRD website. Inside this anchor, it embeds an image (\code{<img>}) element sourced from a specified URL, representing the IRD logo. This panel is intended to be used in a Shiny application UI to provide a clickable logo that navigates to the IRD website.
#'
#' @return A div (\code{div}) HTML element styled to align at the start of its container. This div contains an anchor element wrapping an image element, which displays the IRD logo and links to the IRD website.
#'
#' @importFrom shiny div tags$a tags$img
#' @export
#' 
create_logo_panel <- function() {
  div(
    tags$a(href='https://www.ird.fr/', 
           tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg', 
                    height='89px', width='108px', style="margin: 10px;")),
    style="text-align: start;"
  )
}