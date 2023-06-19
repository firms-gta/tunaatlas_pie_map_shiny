FROM rocker/r-ver:4.2.1

MAINTAINER Julien Barde "julien.barde@ird.fr"

# system libraries of general use
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
    
RUN apt-get update && apt-get upgrade -y
RUN apt-get update && apt-get -y install cmake

#geospatial
RUN /rocker_scripts/install_geospatial.sh

# install R core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 remotes
RUN R -e "install.packages(c('jsonlite','yaml'), repos='https://cran.r-project.org/')"
# clone app
RUN git -C /root/ clone https://github.com/firms-gta/tunaatlas_pie_map_shiny && echo "OK!"
RUN ln -s /root/tunaatlas_pie_map_shiny/srv/tunaatlas_pie_map_shiny
# install R app package dependencies
RUN R -e "source('./srv/tunaatlas_pie_map_shiny/install.R')"

#etc dirs (for config)
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/srv/tunaatlas_pie_map_shiny',port=3838,host='0.0.0.0')"]

RUN apt-get -y update
RUN apt-get install -y curl
