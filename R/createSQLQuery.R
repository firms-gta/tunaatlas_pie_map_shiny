createSQLQuery <- function(dataset_name = default_dataset,
                           species_name = default_species,
                           fishing_fleet_name = default_flag,
                           gear_type_name = default_gear_type,
                           gridtype_name = default_gridtype,
                           selected_years = target_year$year,
                           measurement_unit_name = default_measurement_unit,
                           fishing_mode_name = default_fishing_mode,
                           wkt = global_wkt,
                           con = pool,
                           limit = NULL) {
  
  limit_clause <- if (!is.null(limit)) glue::glue_sql("LIMIT {limit}", .con = con) else SQL("")
  
  query <- glue::glue_sql(
    "SELECT gridtype, codesource_area, species, gear_type, fishing_fleet, SUM(measurement_value) as measurement_value, measurement_unit, fishing_mode, geom,
    ST_asText(geom) AS geom_wkt, year, month FROM public.shinycatch
    WHERE dataset IN ({dataset_name})
    AND ST_Within(geom, ST_GeomFromText(({wkt*}), 4326))
    AND fishing_fleet IN ({fishing_fleet_name*})
    AND species IN ({species_name*})
    AND gear_type IN ({gear_type_name*})
    AND gridtype IN ({gridtype_name*})
    AND year IN ({selected_years*})
    AND fishing_mode IN ({fishing_mode_name*})
    AND measurement_unit IN ({measurement_unit_name*})
    GROUP BY gridtype, species, fishing_fleet, codesource_area, geom_wkt, geom, year, month, gear_type, fishing_mode, measurement_unit
    ORDER BY species, fishing_fleet DESC {limit_clause}",
    .con = pool
  )
  
  flog.info("SQL query created successfully.")
  return(query)
}