#!/bin/bash
set -e

# répertoire app par défaut
APP_DIR="/root/tunaatlas_pie_map_shiny"

# si on est en mode dev, on veut travailler dans /home/rstudio/pour eviter les problemes de cache renv
if [ "$MODE" = "dev" ]; then
  APP_DIR="/home/rstudio/tunaatlas_pie_map_shiny"
fi

# Variables renv (communes aux 2 modes), on force un cache dans le projet
: "${RENV_CONFIG_CACHE_ENABLED:=FALSE}"
: "${RENV_PATHS_ROOT:=${APP_DIR}/renv}"
: "${RENV_PATHS_CACHE:=${APP_DIR}/renv/library}"

mkdir -p "$RENV_PATHS_CACHE" || true

if [ "$MODE" = "dev" ]; then
  echo "🔧 MODE=dev → Préparation de l’environnement RStudio"

  # créer l'utilisateur s'il n'existe pas
  useradd -ms /bin/bash rstudio 2>/dev/null || echo "Utilisateur rstudio déjà présent"

  # si le projet n'est pas encore dans /home/rstudio, on le copie
  if [ ! -d "$APP_DIR" ]; then
    echo "📁 Copie des fichiers vers $APP_DIR"
    mkdir -p "$APP_DIR"
    cp -a /root/tunaatlas_pie_map_shiny/. "$APP_DIR"/
  fi

  # R démarre dans le bon dossier
  echo "setwd('$APP_DIR')" > /home/rstudio/.Rprofile

  # droits
  chown -R rstudio:rstudio /home/rstudio

  echo "🚀 Mode dev : lancement de RStudio Server"
  exec /init
else
  echo "🚀 Mode prod : lancement de l'application Shiny"
  exec R -e "shiny::runApp('$APP_DIR', port=3838, host='0.0.0.0')"
fi
