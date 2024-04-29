# Use the rocker/r-ver:4.2.3 image as the base
FROM rocker/r-ver:4.2.3

# Maintainer information
LABEL maintainer="Julien Barde <julien.barde@ird.fr>, Bastien Grasset <bastien.grasset@ird.fr>"


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
    libudunits2-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    git \
    libnetcdf-dev \
    cmake \
    wget \
    && rm -rf /var/lib/apt/lists/*
#Last line to clean up to prevent the layer from becoming unnecessarily large with cached package data

# Install additional geospatial libraries
RUN /rocker_scripts/install_geospatial.sh

# Set the working directory to /root
WORKDIR /root


## Install renv and restore R packages
#RUN Rscript -e 'install.packages("renv", repos="https://cran.r-project.org/")' \
#    && wget -O renv.lock "https://raw.githubusercontent.com/firms-gta/GlobalTunaAtlasExplorer/CWP_database/renv.lock" \
#    && wget -O Rprofile "https://raw.githubusercontent.com/firms-gta/GlobalTunaAtlasExplorer/CWP_database/.Rprofile" \
#    && wget -O renv/activate.R "https://raw.githubusercontent.com/firms-gta/GlobalTunaAtlasExplorer/CWP_database/renv/activate.R" \
#    && wget -O renv/settings.json "https://raw.githubusercontent.com/firms-gta/GlobalTunaAtlasExplorer/CWP_database/renv/settings.json" \
#     && Rscript -e 'renv::activate()' && Rscript -e 'renv::repair()' && Rscript -e 'renv::restore()'

# Clone the specific branch of the GitHub repository and create a symbolic link
RUN git clone -b CWP_database https://github.com/firms-gta/GlobalTunaAtlasExplorer.git /root/GlobalTunaAtlasExplorer \
    && ln -s /root/GlobalTunaAtlasExplorer /srv/GlobalTunaAtlasExplorer \
    && Rscript -e 'renv::activate()' && Rscript -e 'renv::repair()' && Rscript -e 'renv::restore()'

COPY connection_tunaatlas_inv.txt /root/connection_tunaatlas_inv.txt

# Expose port 3838 for the Shiny app
EXPOSE 3838

# Define the entry point to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/root/GlobalTunaAtlasExplorer', port=3838, host='0.0.0.0')"]
