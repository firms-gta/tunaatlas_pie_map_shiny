ARG MODE=prod
ARG BASE_IMAGE
FROM ${BASE_IMAGE:-rocker/r-ver:4.2.3}

LABEL maintainer="Julien Barde <julien.barde@ird.fr>"

# Install system libraries
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
    redland-utils \
    dos2unix && \
    apt-get clean

RUN install2.r --error --skipinstalled --ncpus -1 httpuv

WORKDIR /root/tunaatlas_pie_map_shiny

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_version('qs', version = '0.26.3', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('jsonlite', version = '1.9.1', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('readr', version = '2.1.5', upgrade = 'never', repos = 'https://cran.r-project.org')"

ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

RUN mkdir -p data 
COPY DOI.csv ./DOI.csv
COPY data/ ./data/

RUN dos2unix DOI.csv && sed -i -e '$a\' DOI.csv

RUN bash -c "tail -n +2 DOI.csv | tr -d '\r' | \
    while IFS=',' read -r DOI FILE; do \
        RECORD_ID=\$(echo \"\$DOI\" | awk -F/ '{print \$NF}' | sed 's/zenodo\\.//'); \
        EXT=\${FILE##*.}; BASE=\${FILE%.*}; \
        ORIGINAL=\"./data/\$FILE\"; \
        if [ \"\$EXT\" = \"qs\" ]; then \
            TARGET=\"./data/\${BASE}_\${RECORD_ID}.qs\"; \
        else \
            TARGET_CSV=\"./data/\${BASE}_\${RECORD_ID}.\${EXT}\"; \
            TARGET=\"\${TARGET_CSV%.*}.qs\"; \
        fi; \
        URL=\"https://zenodo.org/record/\$RECORD_ID/files/\$FILE?download=1\"; \
        if [ -f \"\$TARGET\" ]; then continue; fi; \
        if [ -f \"\$ORIGINAL\" ]; then \
            if [ \"\$EXT\" = \"qs\" ]; then cp \"\$ORIGINAL\" \"\$TARGET\"; \
            else cp \"\$ORIGINAL\" \"\$TARGET_CSV\"; fi; \
        else \
            if [ \"\$EXT\" = \"qs\" ]; then \
                wget -nv -O \"\$TARGET\" \"\$URL\" || { echo \"\$FILE\" >> DOI_failed.csv; continue; } \
            else \
                wget -nv -O \"\$TARGET_CSV\" \"\$URL\" || { echo \"\$FILE\" >> DOI_failed.csv; continue; } \
            fi; \
        fi; \
        if [ \"\$EXT\" != \"qs\" ]; then \
            Rscript -e \"qs::qsave(readr::read_csv('\$TARGET_CSV'), '\$TARGET')\" && rm -f \"\$TARGET_CSV\"; \
        fi; \
    done"

ENV RENV_PATHS_ROOT=/root/.cache/R/renv

# Si en mode dev, changer pour le user rstudio
RUN if [ "$MODE" = "dev" ]; then \
      export NEW_PATH="/home/rstudio/.cache/R/renv" && \
      mkdir -p "$NEW_PATH" && \
      chown -R rstudio:rstudio "$(dirname $NEW_PATH)" && \
      echo "RENV_PATHS_ROOT=$NEW_PATH" >> /etc/environment && \
      echo "RENV_PATHS_CACHE=$NEW_PATH" >> /etc/environment; \
    fi

# Ces variables sont utilisées par R/renv à runtime
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

RUN mkdir -p ${RENV_PATHS_ROOT}
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

#using remotes incase cache keep ancient renv version

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN Rscript -e "remotes::install_version('renv', version = jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version, repos = 'https://cran.r-project.org')"

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache) carreful keep in multiple lines
RUN R -e "renv::restore()" 
RUN R -e "renv::repair()" 

RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data

COPY update_data.R ./update_data.R 
COPY R/load_data.R ./R/load_data.R 

# Run the data update script Downloading the data (cached if DOI.csv did not change).
RUN Rscript update_data.R

COPY R ./R
COPY create_or_load_default_dataset.R ./
RUN Rscript ./create_or_load_default_dataset.R

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
RUN R -e "if (Sys.getenv('BUILD_BRANCH') == 'dev') { library(dplyr); library(here); full <- qs::qread(here::here('data/default_dataset.qs')); full <- full %>% group_by(source_authority, species) %>% slice_head(n=100) %>% ungroup(); qs::qsave(full, here::here('data/default_dataset.qs')) }"

RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

RUN if [ "$MODE" = "dev" ]; then R -e "renv::isolate()"; fi

EXPOSE 3838
EXPOSE 8787

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

RUN if [ "$MODE" = "dev" ] && [ -d "/home/rstudio" ]; then \
      echo "RENV_PATHS_ROOT=/home/rstudio/tunaatlas_pie_map_shiny/renv/library" >> /home/rstudio/.Renviron && \
      echo "RENV_PATHS_CACHE=/home/rstudio/tunaatlas_pie_map_shiny/renv/library" >> /home/rstudio/.Renviron && \
      chown rstudio:rstudio /home/rstudio/.Renviron; \
    fi

ENTRYPOINT ["/entrypoint.sh"]

