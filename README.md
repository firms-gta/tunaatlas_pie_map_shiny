# tunaatlas_pie_map_shiny
Shiny app to display Tuna atlas catches with pie maps

### System Wide Dependendencies
These dependencies can be installed using the usual *sudo apt install* command



### R libraries
**devtools** must be installed in order to install some packages from GitHub
```
install.packages("devtools")
```
The following libraries have to be installed from GitHub
```
devtools::install_github('')
```
The following libraries can be installed from CRAN
```
install.packages(c('shiny', ), repos='https://cloud.r-project.org/')
```

### Docker

#### Pull / Run the image from DockerHub



```
docker pull ghcr.io/firms-gta/tunaatlas_pie_map_shiny

docker run --name tuna_atlas_i11 -p 3839:3838 firms-gta/tunaatlas_pie_map_shiny
```

And then point your browser to http://localhost:3839

Note: In case of having an existing SMT app running on docker, and in order to update the docker app, it will be required to stop and remove the container prior to run the above commands to pull & run the app:

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
