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
RUN R -e "install.packages('remotes', repos='https://cran.r-project.org/')" 
RUN R -e "remotes::install_version('downloader', version = '0.4', upgrade = 'never', repos = 'https://cran.r-project.org/')"
RUN R -e "remotes::install_version('readr', version = '2.1.5', upgrade = 'never',  repos = 'https://cran.r-project.org/')"

# Echo the DOI_CSV_HASH for debugging and to to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

# Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

# Copy the CSV containing the data to download
# Copy the script downloading the data from the CSV
COPY DOI.csv ./DOI.csv 
COPY update_data.R ./update_data.R 

# Run the data update script Downloading the data (cached if DOI.csv did not change).
RUN Rscript update_data.R 
# Some errors due to the timeout may appear, for now fixed by raising the  timeout in yml file

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

# Install renv package that records the packages used in the shiny app
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 

# Copy the rest of the application code
COPY . .  
# attention copy . . invalide le cache, eput expliquer pourquoi create_or_load_default_dataset 
#n'est jamais caché, pourrait copier uniquement les choses utiles pour run la fonction puis le reste

# Create the default dataset from DOI and GTA data loading to make launching faster (use of qs for loading and data.table for tidying) 
RUN Rscript ./create_or_load_default_dataset.R 

COPY . .

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

RUN R -e "library(sf); library(tmap); library(dplyr); library(ggplot2); library(leaflet); library(data.table)"
# Running the library making a lot of time to load as tmap (6 seconds)
# Run the global script to load packages and data prior to running the shiny app 
# Removed as global.R need connection to DB for now to implement everything #not anymore
# RUN Rscript global.R maybe creating errors
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
