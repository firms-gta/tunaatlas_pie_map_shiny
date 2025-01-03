Detailed explenation on filterings, variables and metadatas
================

# Application overview

Please cite this work if you reuse it or find it useful :
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13685479.svg)](https://doi.org/10.5281/zenodo.13685479)

This Shiny application, part of the GlobalTunaAtlas project, is designed
to facilitate the exploration and analysis of worldwide tuna fisheries
data. To begin the exploration, users are invited to explore data and
select desired filters. Upon submission, the application will render the
selected indicators, which may include visual representations such as
maps and graphs. The selection of the dataset to explore can be changed
in the Dataset selection tab and more info about this app and the global
project are displayed in the “More about” panel.

Please be aware that mapping features may experience delays due to data
complexity and applied filters.

## Accessible Data in the Shiny App

This Shiny application provides access to several datasets that allow
for a thorough exploration of data from the Global Tuna Atlas database.
Each dataset comes with a detailed explanation and a DOI identifier,
which can be found in the table below. These details aim to provide you
with precise context and technical insights into the accessible data.

The following dataset can be found under the following DOI. For each
dataset a abstract is provided for more comprehensive choosing. Please
contact us if you need more information.

\`\`\`{r include=FALSE}

require(flextable) require(dplyr)

metadata_dcmi \<-
as.data.frame(readRDS(“\~/tunaatlas_pie_map_shiny/rmd/metadata_dcmi.rds”))
%\>% dplyr::select(Identifier, Title, Description, SpatialCoverage,
TemporalCoverage)


    ```{r echo=FALSE, results='asis'}

    qflextable(metadata_dcmi)

### Recommendations for Data Selection

Currently, we strongly advise opting for `datasetlevel1aggregated5deg`
data. This recommendation is based on the accessibility and the richness
of information these data provide, making exploration and analysis both
simpler and more rewarding.

### Display of Grid Types

It is crucial to note that our application does not support the display
of multiple grid types simultaneously. To maximize the value of the
explored information, we recommend using datasets that are already
aggregated into 5-degree grids. This approach prevents the selection of
a specific grid type, which could lead to a partial loss of available
information.

By choosing aggregated data, you benefit from a comprehensive and
consolidated view, thus facilitating the identification of trends and
patterns within the data.

The repository is a work in progress intended for exploring tuna
fisheries data. It contains publicly available data from Tuna RFMOs. The
content should not be used for publications without explicit agreement
from the authors. Accuracy of estimates depends on data quality and
availability, and may not represent the official view of IRD or its
affiliates. Caution must be taken when interpreting all data presented,
and differences between information products published by IRD and other
sources using different inclusion criteria and different data cut-off
times are to be expected. While steps are taken to ensure accuracy and
reliability, all data are subject to continuous verification and change.
See “Detailed explanations on the datasets used” for further background
and other important considerations surrounding the source data.

This repository has been enriched and supported by the efforts and
contributions of multiple individuals. Their diverse skills and
dedication have been pivotal in the development and success of this
project.

## Main Contributors

- **Julien Barde**
- **Norbert Billet**
- **Emmanuel Chassot**
- **Taha Imzilen**
- **Paul Taconet**
- **Bastien Grasset**
- **UMR MARBEC (Marine Biodiversity, Exploitation and Conservation)**

Each contributor has played a significant role in the repository’s
development, offering unique insights, expertise, and dedication to the
project’s growth.

## Acknowledgments

Special thanks are extended to **Alain Fontenau**, whose original draft
of a set of indicators laid the groundwork for significant portions of
the analysis code. His contributions have been instrumental in guiding
the project’s analytical direction. [Documentation IRD on Global Tuna
Atlas](https://www.documentation.ird.fr/hor/fdi:010012425) [Global Tuna
Atlas
pdf](https://horizon.documentation.ird.fr/exl-doc/pleins_textes/divers11-03/010012425.pdf)

## Indicators creation

The implementation of the indicators into the R programming language was
skillfully executed by **Norbert Billet** and **Julien Barde** during
the iMarine FP7 project. Their expertise in R programming facilitated
the translation of conceptual indicators into practical, executable
code, thereby enabling robust data analysis and insights.

## Shiny app creation

This shiny app as been enriched by Grasset Bastien from an original
repository created by Julien Barde.

For question about the use of the shiny app or about the creation of the
data of the Global Tuna Atlas, feel free to contact us on github or by
email: - <bastien.grasset@ird.fr> - <julien.barde@ird.fr>.

------------------------------------------------------------------------

This document serves to recognize and appreciate the collective efforts
of all individuals involved.

## Running the application on BlueCloud Infrastructure

The app is a component of the GlobalFisheriesAtlas Virtual Research
Environment (VRE), which includes an RStudio server for developers and a
Shiny proxy server for hosting applications. The VRE aims to simplify
data exploration through analytical indicators without the need for
delving into source code. To access this inrastructure you can create an
account into
<https://blue-cloud.d4science.org/group/globalfisheriesatlas> and then
access the shiny application.

## Running the application outside the BlueCloud Infrastructure

### Running from RStudio

Run

``` {r}
shiny::runApp()
```

The app uses the `renv` package for package management. Ensure
compatibility with the R version specified in the lockfile to avoid
loading errors.

#### To access the total available dataset product of the GTA

A connection to a populated database with Global Tuna Atlas data and
metadata is required but no mandatory to observe DOI data. Instructions
for database creation or connection setup are available on the project’s
GitHub page. The connection identifiers must be provided through a
connection_tunaatlas_inv.txt file copied in the repository (or copied in
the docker if using the docker image).

### Running with Docker (recommended)

#### Pull and Run

Pullling the image and running it

``` sh

docker pull ghcr.io/firms-gta/tunaatlas_pie_map_shiny:latest
docker run -p 3838:3838 -v ghcr.io/firms-gta/tunaatlas_pie_map_shiny:latest 
```

#### To run the image passing arguments inside as a DB connection

If you have credential to connect the infrastructure DB or if you have
replicated the database and want to access it with the shiny application

``` bash
docker run -e DB_HOST=mydbhost -e DB_PORT=5432 -e DB_NAME=mydatabase -e DB_USER_READONLY=myuser -e DB_PASSWORD=mypassword tunaatlas_pie_map_shiny
```

Access the app by navigating to <http://localhost:3838> in your browser.

#### Build and Run Locally

If you have cloned the github repository in local you can then:

``` sh
cd the_repo_where_you_pulled_the_shiny_app
docker build -t my-shiny-app .
docker run -p 3838:3838 -v my-shiny-app
```

Access the app by navigating to <http://localhost:3838>.

## Specific dataset to observe

Every dataset need to be CWP format stnadard.

### Ran on Rstudio or if you build the image locally

Create in you repo the default_dataset.qs file wihch contains your
dataset in CWP standard format and in .qs format. Then run the
application or create the docker image

### If you pull the docker image

No easy way found yet. We recommend pulling the repository and building
the docker image in local (see )

This work has been supported by several funding sources, outlined below:

- **Institute of Research for Development (IRD)**
  - The support from IRD has been crucial for conducting our research.

![IRD
Logo](https://raw.githubusercontent.com/juldebar/IRDTunaAtlas/master/logo_IRD.svg)

- **iMarine FP7 Project**
  - Funded under the European Union’s Seventh Framework Programme (FP7).
- **BlueBridge H2020 Project**
  - BlueBridge has received funding from the European Union’s Horizon
    2020 research and innovation programme under grant agreement
    No. 675680.
- **Blue Cloud H2020 Project**
  - Blue-Cloud has been funded by the European Union’s Horizon programme
    call BG-07-2019-2020, topic: \[A\] 2019 - Blue Cloud services, Grant
    Agreement no. 862409. The views and opinions expressed in this
    website are solely the responsibility of the author and do not
    necessarily reflect the views of the European Commission.

This document offers a detailed explanation of the filtering components
used in the Shiny application. The filtering interface allows users to
customize data display based on specific criteria such as year, selected
species, and fishing fleet.

## Usage Instructions

1.  **Select a Dataset:** Begin by choosing the desired dataset.
2.  **Apply Filters:** Specify filters based on provided criteria.
3.  **Submit:** Click submit to view the data visualizations.

## Filterings

### Year Filtering

Users can select a range of year or select several non consecutives
years by clicking on the “Discrete selection of the year” button.

### Species filtering

Users can filter the data based on species. Two action buttons provide
shortcuts for selecting all species or only the major tunas species
being YFT, ALB, BET, SKJ, SBF

### Fishing fleet filtering

A collapsible panel allows users to select one or multiple fishing
fleets. An action button is provided to select all fishing fleets at
once.

### WKT filtering

From all the maps user is allowed to select a wkt to filter data from a
specific region. It is needed to click on submit after this selection to
filter the data.

## Reset and Submission

Users have the option to reset the geographical selection to a global
view or reset all filters to their default values. A submit button sends
the selected filtering criteria to update the data display.

## Future advances

Future filters as month, trimester, gear_type, fishing_mode are to be
added shortly.

## Dockerfile Documentation

### Introduction

This document provides a detailed explanation of the Dockerfile used to
set up and deploy a Shiny application. The Dockerfile includes steps for
configuring the environment, managing dependencies, and running the
Shiny app.

------------------------------------------------------------------------

``` dockerfile
FROM rocker/r-ver:4.2.3
```

- **Base Image**: Uses the `rocker/r-ver` base image with R version
  4.2.3 as the foundation for the container.

------------------------------------------------------------------------

``` dockerfile
LABEL maintainer="Julien Barde <julien.barde@ird.fr>"
```

- **Maintainer Information**: Adds metadata about the person maintaining
  this Dockerfile.

------------------------------------------------------------------------

``` dockerfile
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
```

- **System Dependencies**:
  - Installs system libraries required for building and running the
    Shiny app.
  - Includes dependencies like `pandoc` for document conversion,
    `libssl-dev` for SSL/TLS, and others for geospatial and network
    operations.

------------------------------------------------------------------------

``` dockerfile
RUN install2.r --error --skipinstalled --ncpus -1 httpuv
```

- Installs the `httpuv` R package, which is a core dependency for Shiny
  apps.

------------------------------------------------------------------------

``` dockerfile
WORKDIR /root/tunaatlas_pie_map_shiny
```

- **Set Working Directory**: Specifies the working directory inside the
  container.

------------------------------------------------------------------------

``` dockerfile
COPY download_GTA_data.R ./download_GTA_data.R
RUN Rscript download_GTA_data.R
```

- **Data Preparation**:
  - Copies a script into the container to download necessary datasets,
    from .zip or .csv files on github.
  - Executes the script using `Rscript`.

------------------------------------------------------------------------

``` dockerfile
RUN R -e "install.packages('remotes', repos='https://cran.r-project.org/')" 
RUN R -e "remotes::install_version('downloader', version = '0.4', upgrade = 'never', repos = 'https://cran.r-project.org/')"
RUN R -e "remotes::install_version('readr', version = '2.1.5', upgrade = 'never',  repos = 'https://cran.r-project.org/')"
```

- **Downloading packages for downloading big data**:
  - Download packages to download data from DOI (around 300 MB).
  - This can be done using other packages, it can also be integrated to
    a script.

------------------------------------------------------------------------

``` dockerfile
### Echo the DOI_CSV_HASH for debugging and to to stop cache if DOI.csv has changed (takes in input the hash of the DOI.csv file created in yml)
ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}"

### Create data repository to copy DOI.csv, a file listing the dataset to download from zenodo
RUN mkdir -p data 

### Copy the CSV containing the data to download
### Copy the script downloading the data from the CSV
COPY DOI.csv ./DOI.csv 
COPY update_data.R ./update_data.R 
RUN Rscript update_data.R 
```

This download data from zenodo taking in input a .csv file providing
every DOI of the dataset to be downloaded. As this operation takes time
and bandwidth, we use a cache. For this we check the DOI.csv file and if
it didn’t change this operation is not reran.

``` dockerfile
### ARG defines a constructor argument called RENV_PATHS_ROOT. Its value is passed from the YAML file. An initial value is set up in case the YAML does not provide one
ARG RENV_PATHS_ROOT=/root/.cache/R/renv
ENV RENV_PATHS_ROOT=${RENV_PATHS_ROOT}

### Set environment variables for renv cache
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

### Echo the RENV_PATHS_ROOT for logging
RUN echo "RENV_PATHS_ROOT=${RENV_PATHS_ROOT}"
RUN echo "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}"

### Define the build argument for the hash of renv.lock to stop cache if renv.lock has changed
ARG RENV_LOCK_HASH
RUN echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}"

### Create the renv cache directory
RUN mkdir -p ${RENV_PATHS_ROOT}

### Install renv package that records the packages used in the shiny app
RUN R -e "install.packages('renv', repos='https://cran.r-project.org/')"

### Copy renv configuration and lockfile
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

### Restore renv packages
RUN R -e "renv::activate()" 
### Used to setup the environment (with the path cache)
RUN R -e "renv::restore()" 

### Copy the rest of the application code
COPY . .
```

Similarly, if renv.lock as not changed, all the downloading of the
packages will not be done again as the cache image already contains
them. We then activate the project to use renv and thus to load the
corresonding packages. This action can be put at the all beginning of
the dockerfile if the packages are the input that are the less likely to
change. NB: Any change in the Dockerfile will remove the use of the
cache for the next creation. The ARG RENV_LOCK_HASH corresponds in the
yml to \$(sha256sum renv.lock \| cut -d’ ’ -f1).

``` dockerfile
### Create the default dataset from DOI and GTA data loading to make launching faster (use of qs for loading and data.table for tidying) 
RUN Rscript ./create_or_load_default_dataset.R 
```

Those lines are specific to this shiny app but allows the loading of the
dataset and the tidying to do it before launching the app and thus
reduce the laucning time.

``` dockerfile

### Expose port 3838 for the Shiny app
EXPOSE 3838

### Create directories for configuration
RUN mkdir -p /etc/tunaatlas_pie_map_shiny/

RUN R -e "library(sf); library(tmap); library(dplyr); library(ggplot2); library(leaflet); library(data.table)"
```

Exposure and directories are mandatory for the launching.

``` dockerfile
RUN R -e "library(sf); library(tmap); library(dplyr); library(ggplot2); library(leaflet); library(data.table)"
```

Those lines are to load big packages prior of the launching to avoid
time consuming however it is not clear yet if it works well.

Some good practice can help reducing the time of the launching of the
application, they will be detailled in another documentation file.

### Final Steps

At the end of the Dockerfile: - The `CMD` statement ensures the Shiny
app starts when the container runs:

``` dockerfile
CMD ["R", "-e", "shiny::runApp('/root/tunaatlas_pie_map_shiny', port=3838, host='0.0.0.0')"]
```

- The port `3838` is exposed to allow access to the app.

------------------------------------------------------------------------
