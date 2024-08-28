# Initialize reactive values
flog.info("Initializing reactive values")
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1)
object <- tools::file_path_sans_ext(DOI$Filename[1])

# Load the shapefile
shapefile_path <- here::here("data/cl_areal_grid.csv")
flog.info("Loading shapefile from %s", shapefile_path)
shapefile.fix <- read.csv(shapefile_path)

# Convert to sf object
flog.info("Converting shapefile to sf object")
shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt")
if (is.na(st_crs(shapefile.fix))) {
  flog.warn("No CRS found, setting to WGS84")
  st_crs(shapefile.fix) <- 4326  # Set to WGS84
}

# Set tmap options
flog.info("Setting tmap options")
tmap_options(check.and.fix = TRUE)

flog.info("Filtering and renaming shapefile data")
shapefile.fix <- shapefile.fix[sf::st_is_valid(shapefile.fix),]
shapefile.fix <- shapefile.fix %>% 
  dplyr::rename(cwp_code = CWP_CODE, gridtype = GRIDTYPE, geom = geom_wkt) %>% 
  dplyr::select(cwp_code, gridtype, geom) %>%
  dplyr::mutate(cwp_code = as.character(cwp_code))

flog.info("Converting shapefile to data.table")
shapefile.fix <- as.data.table(shapefile.fix)

flog.info("Loading species data")
species <- read_csv(here::here("data/cl_species.csv")) %>% 
  dplyr::select(code, label, taxa_order) %>% 
  dplyr::rename(code_species = code, species_name = label, species_group = taxa_order) %>% 
  distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv
flog.info("Loaded species and species_group data")

flog.info("Loading cl_cwp_gear_level2 data")
cl_cwp_gear_level2 <- read_csv(here::here("data/cl_cwp_gear_level2.csv")) %>% 
  dplyr::select(Code = code, Gear = label) %>% 
  dplyr::distinct() #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv
flog.info("Loaded cl_cwp_gear_level2 data")

flog.info(sprintf("Time %s:", Sys.time()))

flog.info("Loading default dataset")
default_dataset <- base::get(object)

# Convert species and cl_cwp_gear_level2 to data.table
flog.info("Converting species and cl_cwp_gear_level2 to data.table")
species <- as.data.table(species)
cl_cwp_gear_level2 <- as.data.table(cl_cwp_gear_level2)

# Convert default_dataset to data.table
flog.info("Converting default_dataset to data.table to make all the operations from 30 seconds to 15 seconds")
setDT(default_dataset)

# Perform operations on default_dataset with data.table
flog.info("Filtering and selecting columns in default_dataset")
default_dataset <- default_dataset[
  !is.na(measurement_value), 
  .(year = year(time_start), 
    month = month(time_start), 
    measurement_value, 
    measurement_unit, 
    species, 
    gear_type, 
    source_authority,
    measurement,
    measurement_type,
    geographic_identifier, 
    fishing_mode, 
    fishing_fleet)
]

flog.info("Merging default_dataset with species data")
default_dataset <- merge(default_dataset, species, by.x = "species", by.y = "code_species", all.x = TRUE)

flog.info("Merging default_dataset with cl_cwp_gear_level2 data")
default_dataset <- merge(default_dataset, cl_cwp_gear_level2, by.x = "gear_type", by.y = "Code", all.x = TRUE)
default_dataset[, gear_type := as.character(gear_type)]
default_dataset[, geographic_identifier := as.character(geographic_identifier)]

flog.info("Merging default_dataset with shapefile.fix to add geometry")
default_dataset <- merge(default_dataset, shapefile.fix, by.x = "geographic_identifier", by.y = "cwp_code", all.x = TRUE)
default_dataset <- as.data.frame(default_dataset) %>% dplyr::rename(geom_wkt = geom)

flog.info(sprintf("Time %s:", Sys.time()))
flog.info(sprintf("Colnames %s:", paste0(colnames(default_dataset))))

variable <- c("source_authority",
              "fishing_fleet",
              "species_group", 
              "Gear",
              "gear_type", 
              "species_name",
              "fishing_mode", 
              "measurement_unit",
              "gridtype",
              "species"
)
flog.info(sprintf("Variables: %s", paste0(variable)))

if (exists("pool") && pool::dbIsValid(pool)) {
  flog.info("DB pool exists and is valid")
  
  # load_target_data <- function(file_path) {
  #   target_data <- readRDS(file_path)
  #   list2env(target_data, .GlobalEnv)
  # }
  # 
  # # Call the function to load data
  # load_target_data("data/target.rds")
  # Query distinct values from the database for filters
  
  filters_combinations_query <- glue::glue_sql("SELECT dataset, measurement_unit, gridtype FROM public.shinycatch GROUP BY dataset, measurement_unit, gridtype;",
                                               .con = pool)
  
  flog.info("Executing query to get filters_combinations")
  filters_combinations <- DBI::dbGetQuery(pool, filters_combinations_query)
  
  flog.info("Setting default filter values based on combinations")
  default_dataset_DB <- ifelse('global_catch_5deg_1m_firms_level1' %in% filters_combinations$dataset, "global_catch_5deg_1m_firms_level1", filters_combinations[[1]][1])
  
  default_gridtype <- filters_combinations %>%
    dplyr::filter(dataset == default_dataset_DB) %>%
    head(1) %>%
    pull(gridtype)
  
  default_measurement_unit <- "t"
  
  
}


# shapefile.fix <- st_read(pool,query = "SELECT * from area.cwp_grid") 
# shapefile.fix <- st_read(pool,query = "SELECT * from area.cwp_grid_erased") #https://github.com/fdiwg/fdi-codelists/raw/main/global/cwp/cl_areal_grid.zip
# flog.info("Loaded shapefile.fix data from DB")
# 
# species_group <- st_read(pool, query = "SELECT taxa_order, code FROM species.species_asfis") %>% #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cl_asfis_species.csv
#   dplyr::select(species_group = taxa_order, species = code)%>% dplyr::distinct()
# flog.info("Loaded species_group data from DB")
# 
# cl_cwp_gear_level2 <- st_read(pool, query = "SELECT * FROM gear_type.isscfg_revision_1") %>% #https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_isscfg_gear.csv
#   dplyr::select(Code = code, Gear = label)%>% dplyr::distinct()
# flog.info("Loaded cl_cwp_gear_level2 data from DB")
# 
# query <- sprintf("SELECT gridtype, geom_id, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, geom,
#     ST_asText(geom) AS geom_wkt, year, month FROM public.shinycatch
#     WHERE dataset = '%s'
#     GROUP BY gridtype, species, fishing_fleet, geom_id, geom_wkt, geom, year, month, gear_type, fishing_mode, measurement_unit", default_dataset_DB)
# 
# default_dataset <- DBI::dbGetQuery(pool, query) %>% dplyr::rename(geographic_identifier = geom_id)
