# Use the rocker/r-ver:4.2.3 image as the base
FROM rocker/r-ver:4.2.3

# Maintainer information
LABEL maintainer="Julien Barde <julien.barde@ird.fr>, Bastien Grasset <bastien.grasset@ird.fr>"

# Install system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    git \
    libnetcdf-dev \
    cmake \
    wget \
    && rm -rf /var/lib/apt/lists/*  # Nettoyage pour éviter de surcharger l'image avec des données de cache inutiles

# Installation de bibliothèques géospatiales supplémentaires
RUN /rocker_scripts/install_geospatial.sh

# Définition du répertoire de travail
WORKDIR /root

# Argument pour déterminer si le cache local doit être utilisé
ARG USE_CACHE=false

COPY renv.lock /root/
COPY .Rprofile /root/

# Condition pour restaurer les paquets seulement si le cache n'est pas utilisé
# Activation et restauration de l'environnement renv
RUN if [ "$USE_CACHE" = "false" ]; then \
        Rscript -e "install.packages('renv', repos='https://cran.r-project.org/')"; \
        Rscript -e 'renv::activate(); renv::repair(); renv::restore()'; \
    fi

# Copy the app
COPY . /root/tunaatlas_pie_map_shiny
# Exposition du port 3838 pour l'application Shiny
EXPOSE 3838

# Point d'entrée pour lancer l'application Shiny
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
