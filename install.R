
# Get the list of package names available in the first library path
package_names <- list.files(.libPaths()[1])

# Load all packages available in the first library path
lapply(package_names, library, character.only = TRUE)
