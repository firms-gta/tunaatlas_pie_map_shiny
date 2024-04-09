# Tunaatlas_pie_map_shiny

Shiny app to display Tuna atlas catches with pie maps

# Prequesite

If you want to make this app display the data you need a database populated with Global Tuna Atlas data and metadata. Please follow instruction from https://github.com/firms-gta/geoflow-tunaatlas to create the database or provide a .txt containing connection to a populated database. 

## Running the app from rstudio

To run the app from you rstudio, launch the app.R file.

Currently the shiny app use the renv package to record state of the packages used to run correctly the application. If you are using a different version of R than the one recorded on the lockfile, error loading the packages can happen.

## Running the app from docker

```
docker pull ghcr.io/firms-gta/tunaatlas_pie_map_shiny:latest

docker run --name tuna_atlas_i11 -p 3839:3838 ghcr.io/firms-gta/tunaatlas_pie_map_shiny
```

And then point your browser to http://localhost:3839

Note: In case of having an existing tunaatlas_indicators_pie_map_shiny app running on docker, and in order to update the docker app, it will be required to stop and remove the container prior to run the above commands to pull & run the app:

```
docker container stop tuna_atlas_i11
docker container rm tuna_atlas_i11
```

#### Build / Run the image locally

A Dockerfile is provided and can be used to build up containers with the application.

To build and run the application issue the following commands
```
sudo docker build -t tuna_atlas_i11 <Path of the Dockerfile>
sudo docker run -p 3839:3838 tuna_atlas_i11
```

And then point your browser to http://localhost:3839

As well it can be runned using the command 

```
docker pull ghcr.io/firms-gta/tunaatlas_pie_map_shiny_cwp_database:latest

docker run -p 3838:3838 -v path_to_txt/connection_tunaatlas_inv.txt:/root/tunaatlas_pie_map_shiny/connection_tunaatlas_inv.txt ghcr.io/firms-gta/tunaatlas_pie_map_shiny_cwp_database:latest
```

in a terminal pointing to a .txt that need to contains the following informations:

DB_DRV=
DB_PORT=
DB_HOST=
DB_NAME=
DB_USER_READONLY=
DB_PASSWORD=
