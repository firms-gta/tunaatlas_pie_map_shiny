# syntax=docker/dockerfile:1

ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE}

ARG MODE=prod
ARG BRANCH=main

LABEL maintainer="Julien Barde <julien.barde@ird.fr>"

ENV DEBIAN_FRONTEND=noninteractive \
    MODE=${MODE} \
    BUILD_BRANCH=${BRANCH} \
    RENV_CONFIG_CACHE_ENABLED=FALSE

# Dépendances système nécessaires à l'application R
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-gnutls-dev \
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
    curl \
    udunits-bin \
    gdal-bin \
    libjq-dev \
    cmake \
    protobuf-compiler \
    libprotobuf-dev \
    wget \
    librdf0 \
    librdf0-dev \
    libtbb-dev \
    libzmq3-dev \
    libpoppler-cpp-dev \
    redland-utils \
    dos2unix \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 httpuv

WORKDIR /root/tunaatlas_pie_map_shiny

# Installation de renv avant de copier le code.
# Le cache Docker est réutilisé tant que renv.lock ne change pas.
COPY renv.lock ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN Rscript -e "\
    install.packages( \
        c('remotes', 'jsonlite'), \
        repos = 'https://cloud.r-project.org' \
    ); \
    renv_version <- jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version; \
    remotes::install_version( \
        'renv', \
        version = renv_version, \
        upgrade = 'never', \
        repos = 'https://cloud.r-project.org' \
    )"

# Restauration des dépendances de l'application.
# On ne copie plus renv/library depuis le dépôt.
RUN Rscript -e "renv::restore(prompt = FALSE)"

# Vérification facultative du chargement des packages.
COPY testing_loading_of_all_packages.R ./
RUN Rscript testing_loading_of_all_packages.R

# Copie du code de l'application, mais pas de data/.
COPY R/ R/
COPY global/ global/
COPY modules/ modules/
COPY tab_panels/ tab_panels/
COPY www/ www/
COPY rmd/ rmd/
COPY doc/ doc/

COPY create_or_load_default_dataset.R ./
COPY update_data.R ./
COPY download_GTA_data.R ./

COPY global.R server.R ui.R app_debug.R install.R ./
COPY .here .Rprofile tunaatlas_pie_map_shiny.Rproj ./
COPY .zenodo.json ./
COPY README.md README.Rmd LICENSE ./

# Le répertoire existe, mais reste vide dans cette image.
# Il sera rempli par un volume ou par Dockerfile.imagewithdata.
RUN mkdir -p \
    /root/tunaatlas_pie_map_shiny/data \
    /etc/tunaatlas_pie_map_shiny

# Configuration spécifique à l'image de développement.
# Cette partie suppose que BASE_IMAGE fournit l'utilisateur rstudio.
RUN if [ "${MODE}" = "dev" ]; then \
        id rstudio >/dev/null 2>&1 || { \
            echo "MODE=dev exige une BASE_IMAGE contenant l'utilisateur rstudio"; \
            exit 1; \
        }; \
        mkdir -p \
            /home/rstudio/.cache/R/renv \
            /home/rstudio/tunaatlas_pie_map_shiny/renv/library; \
        printf '%s\n' \
            "RENV_CONFIG_CACHE_ENABLED=FALSE" \
            "RENV_PATHS_ROOT=/home/rstudio/tunaatlas_pie_map_shiny/renv" \
            "RENV_PATHS_CACHE=/home/rstudio/tunaatlas_pie_map_shiny/renv/library" \
            > /home/rstudio/.Renviron; \
        chown -R rstudio:rstudio /home/rstudio; \
    fi

RUN echo "MODE=${MODE}" \
    && echo "BUILD_BRANCH=${BUILD_BRANCH}" \
    && echo "L'image principale ne contient aucune donnée."

EXPOSE 3838
EXPOSE 8787

COPY entrypoint.sh /entrypoint.sh
RUN dos2unix /entrypoint.sh \
    && chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]