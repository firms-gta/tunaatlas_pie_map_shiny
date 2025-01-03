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

require(flextable) require(readr)

metadata_dcmi \<- as.data.frame(read_csv(here::here(“DOI.csv”)))


    ```{r echo=FALSE, results='asis'}

    kable(metadata_dcmi, format = "markdown", caption = "Details of Accessible Datasets")

### Recommendations for data selection

Currently, we strongly advise opting for `datasetlevel1aggregated5deg`
data. This recommendation is based on the accessibility and the richness
of information these data provide, making exploration and analysis both
simpler and more rewarding.

### Display of grid types

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

## Reset and submission

Users have the option to reset the geographical selection to a global
view or reset all filters to their default values. A submit button sends
the selected filtering criteria to update the data display.

## Future advances

Future filters as month, trimester, gear_type, fishing_mode are to be
added shortly.
