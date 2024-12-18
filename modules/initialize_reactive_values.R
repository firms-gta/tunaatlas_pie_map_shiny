# Initialize reactive values
flog.info("Initializing reactive values")
global_wkt <- 'POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))'
wkt <- reactiveVal(global_wkt)
metadata <- reactiveVal() 
zoom <- reactiveVal(1)
default_gridtype <- NULL
default_measurement_unit <- NULL
# Load environment variables from file
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