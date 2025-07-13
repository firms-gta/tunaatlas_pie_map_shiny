#!/bin/bash
set -e

if [ "$MODE" = "dev" ]; then
  echo "🔧 MODE=dev → Préparation de l’environnement RStudio"
  useradd -ms /bin/bash rstudio || echo "Utilisateur rstudio déjà présent"

  if [ ! -d /home/rstudio/tunaatlas_pie_map_shiny ]; then
    echo "📁 Copie des fichiers vers /home/rstudio"
    mkdir -p /home/rstudio/tunaatlas_pie_map_shiny
    cp -a /root/tunaatlas_pie_map_shiny/. /home/rstudio/tunaatlas_pie_map_shiny/
    echo "setwd('/home/rstudio/tunaatlas_pie_map_shiny')" > /home/rstudio/.Rprofile
    chown -R rstudio:rstudio /home/rstudio
  fi

  echo "🚀 Mode dev : lancement de RStudio Server"
  exec /init
else
  echo "🚀 Mode prod : lancement de l'application Shiny"
  exec R -e "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"
fi
