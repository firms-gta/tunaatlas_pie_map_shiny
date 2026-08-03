#!/bin/bash
set -e

# Default application directory.
APP_DIR="/root/tunaatlas_pie_map_shiny"

# In development mode, work under /home/rstudio to keep the RStudio and renv
# paths consistent.
if [ "$MODE" = "dev" ]; then
  APP_DIR="/home/rstudio/tunaatlas_pie_map_shiny"
fi

# Shared renv configuration for production and development modes.
: "${RENV_CONFIG_CACHE_ENABLED:=FALSE}"
: "${RENV_PATHS_ROOT:=${APP_DIR}/renv}"
: "${RENV_PATHS_CACHE:=${APP_DIR}/renv/library}"

mkdir -p "$RENV_PATHS_CACHE" || true

if [ "$MODE" = "dev" ]; then
  echo "MODE=dev: preparing the RStudio environment"

  # Create the development user only when it is absent.
  useradd -ms /bin/bash rstudio 2>/dev/null || echo "The rstudio user already exists"

  # Copy the project into the development home on the first launch.
  if [ ! -d "$APP_DIR" ]; then
    echo "Copying application files to $APP_DIR"
    mkdir -p "$APP_DIR"
    cp -a /root/tunaatlas_pie_map_shiny/. "$APP_DIR"/
  fi

  # Start R in the application directory.
  echo "setwd('$APP_DIR')" > /home/rstudio/.Rprofile

  # Give the development user ownership of its home directory.
  chown -R rstudio:rstudio /home/rstudio

  echo "Starting RStudio Server in development mode"
  exec /init
else
  echo "Starting the Shiny application in production mode"
  exec R -e "shiny::runApp('$APP_DIR', port=3838, host='0.0.0.0')"
fi