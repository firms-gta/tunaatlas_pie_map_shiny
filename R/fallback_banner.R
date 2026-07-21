fallback_banner <- function() {
  if (isTRUE(getOption("app_using_fallback_dataset", FALSE))) {
    shiny::div(
      class = "alert alert-warning",
      shiny::tags$strong(
        "⚠️ You are currently viewing a minimal demo dataset."
      ),
      shiny::tags$br(),
      "The full Tuna Atlas datasets are not loaded. ",
      "To access all available datasets, either deploy an image with a ",
      "preloaded dataset or connect the application to the database."
    )
  } else {
    NULL
  }
}