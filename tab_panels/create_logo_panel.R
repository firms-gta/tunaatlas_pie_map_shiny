create_logo_panel <- function() {
  div(
    tags$a(href='https://www.ird.fr/', 
           tags$img(src='https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg', 
                    height='89px', width='108px', style="margin: 10px;")),
    style="text-align: start;"
  )
}