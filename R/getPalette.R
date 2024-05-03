#' Retrieve a Specific Color Palette
#'
#' This function returns the color palette for the specified category, if it exists.
#'
#' @param category The name of the category for which the color palette is requested.
#'
#' @return A color vector for the specified category.
#' @examples
#' palette_species <- getPalette("species")
#' @export
#' 
getPalette <- function(category) {
  if (!is.null(palettes[[category]])) {
    return(palettes[[category]])
  } else {
    stop("Unknown category. Be sure of the existance of this palette.")
  }
}
