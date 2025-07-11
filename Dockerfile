ARG BASE_IMAGE=rocker/r-ver:4.2.3
FROM ${BASE_IMAGE}

# Maintainer information
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"

# Install system libraries of general use
# Install protobuf libraries
# Install additional libraries for redland
# libcurl4-openssl-dev is to install libraptor2-dev ued to install protobuf

RUN apt-get update && apt-get install -y \
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
    redland-utils && \
    apt-get clean
    
# Be Careful, I think the comments shouldn't be on the same line than the instruction of the dockerfile as it creates some errors

# Update and upgrade the system
RUN apt-get update && apt-get upgrade -y

RUN apt-get update && apt-get install -y dos2unix

# Install R core package dependencies the following line install httpuv that is usually used in shiny apps
RUN install2.r --error --skipinstalled --ncpus -1 httpuv

# Set the working directory
WORKDIR /root/tunaatlas_pie_map_shiny

#those packages are essential to download the data in update_data.R, they are ran before renv because the renv.lock would change more than the DOI2.csv
RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_version('qs', version = '0.26.3', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('jsonlite', version = '1.9.1', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('readr', version = '2.1.5', upgrade = 'never', repos = 'https://cran.r-project.org')"

# Echo the DOI_CSV_HASH for debugging and to to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

# Copy the CSV containing the data to download
# Copy the script downloading the data from the CSV
COPY DOI.csv ./DOI.csv

# Appliquer dos2unix pour éviter les problèmes de formatage
RUN dos2unix DOI.csv && cat -A ./DOI.csv

# 1) Assurer le \n à la fin de DOI.csv
RUN sed -i -e '$a\' DOI.csv

COPY data/ ./data/

# 2) Télécharger, renommer, convertir puis nettoyer
RUN echo "📥 Downloading and converting files..." && \
    bash -c "tail -n +2 DOI.csv | tr -d '\r' | \
    while IFS=',' read -r DOI FILE; do \
        # Extraire l'ID Zenodo et les noms
        RECORD_ID=\$(echo \"\$DOI\" | awk -F/ '{print \$NF}' | sed 's/zenodo\\.//'); \
        EXT=\${FILE##*.}; BASE=\${FILE%.*}; \
        ORIGINAL=\"./data/\$FILE\"; \
        # Pour CSV/RDS on télécharge/renomme vers NEWNAME, sinon pour QS directement vers FINAL_QS
        if [ \"\$EXT\" = \"qs\" ]; then \
            TARGET=\"./data/\${BASE}_\${RECORD_ID}.qs\"; \
        else \
            TARGET_CSV=\"./data/\${BASE}_\${RECORD_ID}.\${EXT}\"; \
            TARGET=\"\${TARGET_CSV%.*}.qs\"; \
        fi; \
        URL=\"https://zenodo.org/record/\$RECORD_ID/files/\$FILE?download=1\"; \
        echo \"➡️ Processing \$FILE (ID=\$RECORD_ID) → \$TARGET\"; \
        # 1) Si le .qs final existe -> skip
        if [ -f \"\$TARGET\" ]; then \
            echo \"   ✅ Already exists: \$TARGET\"; \
            continue; \
        fi; \
        # 2) Récupérer le source local ou télécharger
        if [ -f \"\$ORIGINAL\" ]; then \
            echo \"   📦 Local copy found: \$ORIGINAL\"; \
            if [ \"\$EXT\" = \"qs\" ]; then \
                cp \"\$ORIGINAL\" \"\$TARGET\"; \
            else \
                cp \"\$ORIGINAL\" \"\$TARGET_CSV\"; \
            fi; \
        else \
            echo \"   🌐 Downloading from Zenodo\"; \
            if [ \"\$EXT\" = \"qs\" ]; then \
                wget -nv -O \"\$TARGET\" \"\$URL\" || { echo \"   ⚠️ Download failed: \$FILE\" >> DOI_failed.csv; continue; } \
            else \
                wget -nv -O \"\$TARGET_CSV\" \"\$URL\" || { echo \"   ⚠️ Download failed: \$FILE\" >> DOI_failed.csv; continue; } \
            fi; \
        fi; \
        # 3) Si besoin, convertir en .qs
        if [ \"\$EXT\" != \"qs\" ]; then \
            Rscript -e \"qs::qsave(readr::read_csv('\$TARGET_CSV'), '\$TARGET')\"; \
            rm -f \"\$TARGET_CSV\"; \
        fi; \
    done"


    
RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data

# ARG defines a constructor argument called RENV_PATHS_ROOT. Its value is passed from the YAML file. An initial value is set up in case the YAML does not provide one
ARG RENV_PATHS_ROOT=/root/.cache/R/renv
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

# Set environment variables for renv cache
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

# Echo the RENV_PATHS_ROOT for logging
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}"
RUN echo "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}"

# Define the build argument for the hash of renv.lock to stop cache if renv.lock has changed
ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

# Create the renv cache directory
RUN mkdir -p ${RENV_PATHS_ROOT}

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

RUN R -e "lockfile <- jsonlite::fromJSON('renv.lock'); renv_version <- lockfile$Packages[['renv']]$Version; install.packages('renv', repos='https://cran.r-project.org/', type='source', version=renv_version)"

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 
RUN R -e "renv::repair()" 

RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data

COPY update_data.R ./update_data.R 
COPY R/load_data.R ./R/load_data.R 

# Run the data update script Downloading the data (cached if DOI.csv did not change).
RUN Rscript update_data.R 

# Copy the rest of the application code
COPY R ./R
COPY create_or_load_default_dataset.R ./create_or_load_default_dataset.R 
# attention copy . . invalide le cache, eput expliquer pourquoi create_or_load_default_dataset 
#n'est jamais caché, pourrait copier uniquement les choses utiles pour run la fonction puis le reste

# Create the default dataset from DOI and GTA data loading to make launching faster (use of qs for loading and data.table for tidying) 
RUN Rscript ./create_or_load_default_dataset.R 

# Copy the rest of the application code
COPY global.R server.R ui.R app_debug.R install.R ./
COPY download_GTA_data.R ./
COPY modules/ modules/
COPY tab_panels/ tab_panels/
COPY www/ www/
COPY .here .Rprofile tunaatlas_pie_map_shiny.Rproj ./
COPY .zenodo.json ./
COPY README.md README.Rmd LICENSE ./
COPY rmd/ rmd/
COPY global/ global/
COPY doc/ doc/
COPY ./Dockerfile.multistage ./

ENV BUILD_BRANCH=${BRANCH}

RUN R -e '\
  if (Sys.getenv("BUILD_BRANCH") == "dev") { \
    library(dplyr); \
    library(here); \
    full <- qs::qread(here::here("data/default_dataset.qs")); \
    message("dev build: head by groups"); \
    full <- full %>% dplyr::group_by(source_authority, species) %>% dplyr::slice_head(n=100) %>% dplyr::ungroup(); \
    qs::qsave(full, here::here("data/default_dataset.qs")); \
  } \
'

# Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

# Define the entry point to run the Shiny appEXPOSE 3838
EXPOSE 3838
EXPOSE 8787

# Commande conditionnelle
CMD if [ "$MODE" = "dev" ]; then \
    /usr/lib/rstudio-server/bin/rserver --server-daemonize 0; \
    else \
    R -e "shiny::runApp('/home/rstudio/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"; \
    fi
    
    
    