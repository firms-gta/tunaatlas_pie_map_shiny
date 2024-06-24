FROM rocker/r-ver:4.2.3

# Maintainer information
MAINTAINER Julien Barde "julien.barde@ird.fr"

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
    curl

# Update and upgrade the system
RUN apt-get update && apt-get upgrade -y

# Install cmake
RUN apt-get update && apt-get -y install cmake

# Install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes', 'jsonlite', 'yaml'), repos='https://cran.r-project.org/')"

# Install renv package
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Set environment variables for renv cache
ARG RENV_PATHS_ROOT=/root/tunaatlas_pie_map_shiny/renv/.cache
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}
RUN mkdir -p ${RENV_PATHS_ROOT}

# Set the working directory
WORKDIR /root/tunaatlas_pie_map_shiny

# Copy renv configuration and lockfile
COPY renv.lock ./
COPY .Rprofile ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

# Set renv cache location
ENV RENV_PATHS_CACHE renv/.cache

# Restore renv packages
RUN R -e "renv::activate()"
RUN R -e "renv::restore()"

# Copy the rest of the application code
COPY . .

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
