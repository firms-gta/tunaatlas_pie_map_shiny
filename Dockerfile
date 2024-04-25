# Use the rocker/r-ver:4.2.3 image as the base
FROM rocker/r-ver:4.2.3

# Maintainer information
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"

# Update and install system dependencies
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
    wget

# Install additional geospatial libraries
RUN /rocker_scripts/install_geospatial.sh

# Install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
RUN R -e "install.packages(c('remotes','jsonlite','yaml'), repos='https://cran.r-project.org/')"

# Set the working directory to /root
WORKDIR /root

# Install git and wget if not already installed
RUN apt-get update && apt-get install -y git wget

# Install renv and restore R packages
RUN Rscript -e 'install.packages("renv", repos="https://cran.r-project.org/")' \
    && wget -O renv.lock "https://raw.githubusercontent.com/firms-gta/tunaatlas_pie_map_shiny/CWP_database/renv.lock" \
    && Rscript -e 'renv::restore()'

# Clone the specific branch of the GitHub repository and create a symbolic link
RUN git clone -b CWP_database https://github.com/firms-gta/tunaatlas_pie_map_shiny.git /root/tunaatlas_pie_map_shiny \
    && ln -s /root/tunaatlas_pie_map_shiny /srv/tunaatlas_pie_map_shiny

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
