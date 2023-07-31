
# Get the list of package names available in the first library path
package_names <- list.files(.libPaths()[1])

# Load all packages available in the first library path
lapply(package_names, library, character.only = TRUE)

# renv::init(force = TRUE)
# renv::restore()
# 
# if (!requireNamespace("here", quietly = TRUE)) {
#   install.packages("here")
# }
# 
# library(here)
# setwd(here())  # Set the working directory to the root of the project

packages <- jsonlite::read_json("package.json")
#
#
for (package_info in packages$dependencies) {
  pkg <- package_info$package
  package_version <- package_info$version
  package_repos <- package_info$repos

  # Install and load the package (if not already installed)
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, version = package_version, repos = package_repos)
    require(pkg, character.only = TRUE)
  }
}

# Load packages
# invisible(lapply(packages$dependencies, function(pkg) {
#   browser()
#   if (!requireNamespace(pkg$package, quietly = TRUE)) {
#     from <- 'cran'
#     pkg_installer <- remotes::install_version
#     if (!is.null(pkg$from)) {
#       from <- pkg$from
#       pkg_installer <- try(eval(parse(text = paste0("remotes::install_", from))))
#     }
#     if (class(pkg_installer)[1] == "try-error") return(NULL)
#     version <- ""
#     if (!is.null(pkg$version)) version <- paste0("[", pkg$version, "]")
#     cat(sprintf("Installing package '%s' %s from '%s'\n", pkg$package, version, from))
#     pkg_args <- pkg[!names(pkg) %in% c("package", "from", "dependencies")]
#     pkg_deps <- NULL
#     if (!is.null(pkg$dependencies)) pkg_deps <- pkg$dependencies
#     do.call(pkg_installer, c(pkg_args, dependencies = pkg_deps))
#   }
# }))
# 


# # Activate the renv environment (if not already activated)
# if (!"renv" %in% names(sessionInfo()$otherPkgs)) {
#   renv::activate()
# }
# 
# # Automatically load all packages from the renv.lock file
# renv::hydrate(prompt = FALSE)
# 
# # Attempt to repair broken symlinks and missing packages
# renv::repair()
# 
# # Restore package dependencies using renv
# renv::restore()
# 
# install.packages("htmltools")
# require(htmltools)