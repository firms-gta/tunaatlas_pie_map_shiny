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
  if(!"geographic_identifier" %in% colnames(geom)){
    geom$geographic_identifier <- geom$code 
  }
  flog.info("geometry of preloaded data removed")
  ids <- unique(default_dataset_preloaded_without_geom$geographic_identifier)
  
  geom <- geom[geom$geographic_identifier %in% ids, ]
  
  list(initial_data = geom,
    data_for_filters = default_dataset_preloaded_without_geom
  )
}

load_query_data <- function(
    selected_dataset,
    selected_gridtype,
    selected_measurement_unit,
    selected_view = "public.shinycatch",
    debug,
    pool
) {
  
  view_name <- as.character(selected_view)
  
  # Expected dimensions for each database view
  wanted_dimensions <- switch(
    view_name,
    "public.shinycatch" = c(
      "gridtype",
      "codesource_area",
      "species",
      "gear_type",
      "fishing_fleet",
      "measurement_unit",
      "fishing_mode",
      "year",
      "month",
      "source_authority"
    ),
    "public.issueddata" = c(
      "gridtype",
      "codesource_area",
      "species",
      "gear_type",
      "fishing_fleet",
      "measurement_unit",
      "fishing_mode",
      "year",
      "month",
      "source_authority",
      "issue"
    ),
    "public.shinyeffort" = c(
      "gridtype",
      "codesource_area",
      "gear_type",
      "fishing_fleet",
      "measurement_unit",
      "fishing_mode",
      "year",
      "month"
    ),
    stop("Provide a valid view to request")
  )
  
  # The view name has been validated by switch()
  selected_view <- DBI::SQL(view_name)
  
  # Retrieve column names without loading data
  available_columns <- names(
    DBI::dbGetQuery(
      pool,
      glue::glue_sql(
        "SELECT * FROM {selected_view} LIMIT 0",
        .con = pool
      )
    )
  )
  
  flog.info(
    "Columns available in %s: %s",
    view_name,
    paste(available_columns, collapse = ", ")
  )
  
  # Columns required by the query and the application
  required_columns <- c(
    "dataset",
    "gridtype",
    "codesource_area",
    "measurement_value",
    "measurement_unit",
    "year"
  )
  
  missing_required <- setdiff(
    required_columns,
    available_columns
  )
  
  if (length(missing_required) > 0L) {
    stop(
      "Required columns missing from ",
      view_name,
      ": ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  # Keep only dimensions that are actually available
  group_columns <- intersect(
    wanted_dimensions,
    available_columns
  )
  
  missing_optional <- setdiff(
    wanted_dimensions,
    available_columns
  )
  
  if (length(missing_optional) > 0L) {
    flog.warn(
      "Columns absent from %s and ignored: %s",
      view_name,
      paste(missing_optional, collapse = ", ")
    )
  }
  
  group_columns_sql <- DBI::SQL(
    paste(group_columns, collapse = ", ")
  )
  
  base_query <- "
    SELECT {group_columns_sql},
           SUM(measurement_value) AS measurement_value
    FROM {selected_view}
    WHERE dataset = {selected_dataset}
      AND gridtype IN ({selected_gridtype*})
      AND measurement_unit IN ({selected_measurement_unit*})
    GROUP BY {group_columns_sql}"
  
  if (debug) {
    base_query <- paste(base_query, "LIMIT 10000")
  }
  
  query <- glue::glue_sql(
    base_query,
    selected_dataset = selected_dataset,
    selected_gridtype = selected_gridtype,
    selected_measurement_unit = selected_measurement_unit,
    selected_view = selected_view,
    group_columns_sql = group_columns_sql,
    .con = pool
  )
  
  flog.info("%s", as.character(query))
  
  geom_query <- glue::glue_sql(
    "
    SELECT DISTINCT codesource_area
    FROM {selected_view}
    WHERE dataset = {selected_dataset}
      AND gridtype IN ({selected_gridtype*})
      AND measurement_unit IN ({selected_measurement_unit*})
    ",
    selected_dataset = selected_dataset,
    selected_gridtype = selected_gridtype,
    selected_measurement_unit = selected_measurement_unit,
    selected_view = selected_view,
    .con = pool
  )
  
  geom <- qs::qread("data/cl_areal_grid.qs")
  
  geometry <- DBI::dbGetQuery(pool, geom_query) %>%
    dplyr::rename(
      geographic_identifier = codesource_area
    )
  
  geom <- geom %>%
    dplyr::semi_join(
      geometry,
      by = "geographic_identifier"
    )
  
  data <- DBI::dbGetQuery(pool, query) %>%
    dplyr::rename(
      geographic_identifier = codesource_area
    ) %>%
    dplyr::mutate(
      measurement_unit = dplyr::case_when(
        measurement_unit == "t" ~ "Tons",
        measurement_unit == "no" ~ "Number of fish",
        TRUE ~ measurement_unit
      )
    )
  
  # Annual datasets may not contain a month column
  if (!"month" %in% colnames(data)) {
    data$month <- 1L
  }
  
  list(
    initial_data = geom,
    data_for_filters = data
  )
}
