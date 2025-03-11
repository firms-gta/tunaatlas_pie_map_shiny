FROM rocker/r-ver:4.2.3

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
    libjq-dev \
    cmake \
    protobuf-compiler \
    libprotobuf-dev \
    librdf0 \
    librdf0-dev \
    redland-utils && \
    apt-get clean
    
# Be Careful, I think the comments shouldn't be on the same line than the instruction of the dockerfile as it creates some errors

# Update and upgrade the system
RUN apt-get update && apt-get upgrade -y

# Install R core package dependencies the following line install httpuv that is usually used in shiny apps
RUN install2.r --error --skipinstalled --ncpus -1 httpuv

# Set the working directory
WORKDIR /root/tunaatlas_pie_map_shiny

COPY download_GTA_data.R ./download_GTA_data.R

# Run the data to donwload GTA data for species label, species group, cwp_shape
RUN Rscript download_GTA_data.R

#those packages are essential to download the data in update_data.R, they are ran before renv because the renv.lock would change more than the DOI2.csv
RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')" \
    && Rscript -e "remotes::install_version('qs', version = '0.26.3', upgrade = 'never', repos = 'https://cran.r-project.org/')" \
    && Rscript -e "remotes::install_version('readr', version = '2.1.5', upgrade = 'never', repos = 'https://cran.r-project.org/)"

# Echo the DOI_CSV_HASH for debugging and to to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

# Copy the CSV containing the data to download
# Copy the script downloading the data from the CSV
COPY DOI.csv ./DOI.csv

# Appliquer dos2unix pour éviter les problèmes de formatage
RUN dos2unix ./DOI.csv && cat -A ./DOI.csv

# Télécharger les fichiers depuis Zenodo
RUN echo "📥 Downloading files..." \
    && bash -c "tail -n +2 ./DOI.csv | tr -d '\r' | while IFS=',' read -r DOI FILE; do \
        RECORD_ID=\$(echo \"\$DOI\" | awk -F '/' '{print \$NF}' | sed 's/zenodo\\.//'); \
        FILE_PATH=\"./data/\$FILE\"; \
        NEWNAME=\"./data/\${FILE%.*}_\${RECORD_ID}.\${FILE##*.}\"; \
        URL=\"https://zenodo.org/record/\$RECORD_ID/files/\$FILE?download=1\"; \
        
        echo \"📥 Downloading \$FILE (Record ID: \$RECORD_ID)\"; \
        
        if wget -nv --retry-connrefused --waitretry=5 --timeout=600 --tries=1 -O \"\$FILE_PATH\" \"\$URL\"; then \
            mv \"\$FILE_PATH\" \"\$NEWNAME\"; \
            echo \"✅ File downloaded and renamed: \$NEWNAME\"; \
            Rscript -e 'qs::qsave(readr::read_csv(Sys.getenv(\"NEWNAME\")), sub(\"\\\\..*\", \".qs\", Sys.getenv(\"NEWNAME\")))' \
            && rm \"\$NEWNAME\"; \
        else \
            echo \"⚠️ Download failed, adding to failed list\"; \
            echo \"\$DOI,\$FILE\" >> ./DOI_failed.csv; \
        fi; \
    done"

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

COPY update_data.R ./update_data.R 

# Run the data update script Downloading the data (cached if DOI.csv did not change).
RUN Rscript update_data.R 

# Copy the rest of the application code
COPY . .

# Create the default dataset from DOI and GTA data loading to make launching faster (use of qs for loading and data.table for tidying) 
RUN Rscript ./create_or_load_default_dataset.R 

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
