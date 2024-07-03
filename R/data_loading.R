# R/data_loading.R

load_initial_data <- function(debug, default_dataset_preloaded, pool) {
  if (!debug || is.null(default_dataset_preloaded)) {
    default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
    flog.info("Data sf loaded")
  }
  
  default_dataset_preloaded_without_geom <- default_dataset_preloaded
  default_dataset_preloaded_without_geom$geom <- NULL
  default_dataset_preloaded_without_geom$geom_wkt <- NULL
  
  list(
    initial_data = default_dataset_preloaded,
    data_for_filters = default_dataset_preloaded_without_geom
  )
}

load_query_data <- function(selected_dataset, selected_gridtype, selected_measurement_unit, debug, pool) {
  base_query <- "
    SELECT gridtype, geom_id, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, geom, ST_AsText(geom) AS geom_wkt, year, month 
    FROM public.shinycatch 
    WHERE dataset = {selected_dataset} AND gridtype = {selected_gridtype} AND measurement_unit = {selected_measurement_unit}
    GROUP BY gridtype, species, fishing_fleet, geom_id, geom_wkt, geom, year, month, gear_type, fishing_mode, measurement_unit"
  
  if (debug) {
    base_query <- paste(base_query, "LIMIT 10000")
  }
  
  query <- glue::glue_sql(base_query, 
                          selected_dataset = selected_dataset, 
                          selected_gridtype = selected_gridtype, 
                          selected_measurement_unit = selected_measurement_unit, 
                          .con = pool)
  
  data <- dbGetQuery(pool, query)
  data_sf <- as.data.frame(st_as_sf(data, wkt = "geom_wkt", crs = 4326))
  
  initial_data <- data_sf
  initial_data$geom <- NULL
  data_for_filters <- initial_data
  
  list(
    initial_data = data_sf,
    data_for_filters = data_for_filters
  )
}
