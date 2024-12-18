# R/data_loading.R

load_initial_data <- function(default_dataset_preloaded) {
  if (is.null(default_dataset_preloaded)) {
    flog.info("No default dataset preloaded using default_dataset.qs")
    
    default_dataset_preloaded <- qs::qread(here::here("data/default_dataset.qs"))
    flog.info("Data sf loaded")
  } else {
    flog.info("Default dataset preloaded using this one")
    
    default_dataset_preloaded <- default_dataset_preloaded
    
  }
  flog.info("Creating dataset with geometry only")
  geom <- qs::qread("data/cl_areal_grid.qs")
  # geom <- default_dataset_preloaded %>% 
  #   dplyr::select(geom_wkt, geographic_identifier) %>% dplyr::distinct()
  flog.info("Dataset with geometry only created")
  flog.info("Removing geometry of preloaded data")
  default_dataset_preloaded_without_geom <- default_dataset_preloaded
  # if ("geom" %in% colnames(default_dataset_preloaded_without_geom)) {
  #   default_dataset_preloaded_without_geom <- default_dataset_preloaded_without_geom %>% dplyr::select(-geom)
  # }
  default_dataset_preloaded_without_geom$geom_wkt <- NULL
  default_dataset_preloaded_without_geom$geom <- NULL
  flog.info("geometry of preloaded data removed")
  geom <- geom %>% dplyr::semi_join(default_dataset_preloaded_without_geom, by = "geographic_identifier")
  list(initial_data = geom,
    data_for_filters = default_dataset_preloaded_without_geom
  )
}

load_query_data <- function(selected_dataset, selected_gridtype, selected_measurement_unit, debug, pool) {
  base_query <- "
    SELECT gridtype, codesource_area, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, year, month 
    FROM public.shinycatch 
    WHERE dataset = {selected_dataset}
    AND gridtype IN ({selected_gridtype*})
    AND measurement_unit IN ({selected_measurement_unit*})
    GROUP BY gridtype, species, fishing_fleet, codesource_area, year, month, gear_type, fishing_mode, measurement_unit"
  
  if (debug) {
    base_query <- paste(base_query, "LIMIT 10000")
  }
  
  query <- glue::glue_sql(base_query, 
                          selected_dataset = selected_dataset, 
                          selected_gridtype = selected_gridtype, 
                          selected_measurement_unit = selected_measurement_unit, 
                          .con = pool)
  geom_query <- glue_sql("
  SELECT DISTINCT codesource_area
  FROM public.shinycatch
  WHERE dataset = {selected_dataset}
    AND gridtype IN ({selected_gridtype*})
    AND measurement_unit IN ({selected_measurement_unit*})",
                         .con = pool)  
  geom_query <- glue::glue_sql(geom_query, 
                          selected_dataset = selected_dataset, 
                          selected_gridtype = selected_gridtype, 
                          selected_measurement_unit = selected_measurement_unit, 
                          .con = pool)
  
  geom <- qs::qread("data/cl_areal_grid.qs")
  geometry <- dbGetQuery(pool, geom_query) %>% dplyr::rename(geographic_identifier = codesource_area) 
  geom <-geom %>% dplyr::semi_join(geometry, by = "geographic_identifier")
  
  data <- dbGetQuery(pool, query) %>% dplyr::rename(geographic_identifier = codesource_area)%>% 
    dplyr::mutate(measurement_unit = case_when(measurement_unit =="t"~"Tons", 
                                               measurement_unit == "no" ~ "Number of fish",
                                               TRUE ~ measurement_unit))
  # data_sf <- as.data.frame(st_as_sf(data, wkt = "geom_wkt", crs = 4326))
  # 
  # initial_data <- data_sf
  # initial_data$geom <- NULL
  # data_for_filters <- initial_data
  
  list(initial_data = geom,
       data_for_filters = data
  )
}
