#' Include a Child Markdown Document with Lowered Heading Levels
#'
#' This function reads a child Markdown document, lowers the level of all Markdown headings
#' by a specified amount, and then includes the processed content in the parent R Markdown document.
#' It is useful for integrating content from separate Markdown files while maintaining
#' a coherent structure in the parent document.
#'
#' @param child_path A character string specifying the path to the child Markdown document.
#' @param lower_by An integer indicating how many levels to lower the Markdown headings. The default is 1.
#'
#' @return The function does not return a value. Instead, it outputs the processed Markdown content
#' directly into the parent document.
#'
#' @examples
#' \dontrun{
#' include_child_with_lowered_headings("path/to/your/child_document.Rmd", lower_by = 2)
#' }
#'
#' @importFrom stringr str_replace_all
#' @export
#'
#' @note This function directly manipulates text and may not handle edge cases, such as headings
#' within code blocks or other non-standard uses of Markdown. It is designed for standard Markdown
#' heading usage.
include_child_with_lowered_headings <- function(child_path, lower_by = 1) {
  library(stringr)
  
  # Ensure the path is a character string
  if (!is.character(child_path)) {
    stop("child_path must be a character string.")
  }
  
  # Validate lower_by parameter
  if (!is.numeric(lower_by) || lower_by < 1) {
    stop("lower_by must be a positive integer.")
  }
  
  # Read the content of the child Markdown file
  child_content <- readLines(child_path, warn = FALSE)
  
  # Check if file reading was successful
  if (length(child_content) == 0) {
    stop("Failed to read the file or the file is empty.")
  }
  
  # Prepare the string of #'s to add
  hash_to_add <- strrep("#", lower_by)
  
  # Add the additional '#' to all Markdown headings
  modified_content <- str_replace_all(child_content, "^(#+)", paste0("\\1", hash_to_add))
  
  # Combine the lines back into a single string
  modified_content <- paste(modified_content, collapse="\n")
  
  # Use knit_expand to dynamically include the modified content
  cat(knitr::knit_expand(text = modified_content))
}
