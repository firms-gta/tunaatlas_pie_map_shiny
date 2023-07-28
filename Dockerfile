# Use the rocker/r-ver:4.2.2 image as the base
FROM rocker/r-ver:4.3.1

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
    git

# Update and upgrade the system
RUN apt-get update && apt-get upgrade -y

# Install cmake
RUN apt-get update && apt-get -y install cmake

# Install additional geospatial libraries
RUN /rocker_scripts/install_geospatial.sh

# Install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes','jsonlite','yaml'), repos='https://cran.r-project.org/')"

# Set the working directory to /root
WORKDIR /root

# Copy everything from the current directory (project directory) to /root/tunaatlas_pie_map_shiny
ADD . /root/tunaatlas_pie_map_shiny

# Install renv package
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

# Set the working directory to /root/tunaatlas_pie_map_shiny
WORKDIR /root/tunaatlas_pie_map_shiny

# Attempt to repair broken symlinks and missing packages
RUN R -e "renv::repair()"

# Restore package dependencies using renv
RUN R -e "renv::restore()"

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]

# Update and install curl (if needed)
RUN apt-get -y update
RUN apt-get install -y curl
