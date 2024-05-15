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
    libnetcdf-dev
    
# Install additional geospatial libraries
RUN /rocker_scripts/install_geospatial.sh

# Install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes', 'jsonlite', 'yaml'), repos='https://cran.r-project.org/')"

# Install renv package
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Set the working directory
WORKDIR /root/tunaatlas_pie_map_shiny

RUN mkdir -p renv
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# change default location of cache to project folder
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE renv/.cache

# restore 
RUN R -e "renv::restore()"

# Copy the rest of the application code
COPY . /root/tunaatlas_pie_map_shiny

# Create a symbolic link to the application directory
RUN ln -s /root/tunaatlas_pie_map_shiny /srv/tunaatlas_pie_map_shiny

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/
  
# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]

# Update and install curl (if needed)
RUN apt-get -y update
RUN apt-get install -y curl