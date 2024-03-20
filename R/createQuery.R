createQuery <- function(pool, wkt, species, fleet, startYear, endYear) {
  query <- glue::glue_sql(
    "SELECT geom_id, geom, species, fishing_fleet, SUM(measurement_value) AS measurement_value, 
     ST_asText(geom) AS geom_wkt, year 
     FROM public.i6i7i8 
     WHERE ST_Within(geom, ST_GeomFromText({wkt}, 4326)) 
     AND fishing_fleet IN ({fleet*}) 
     AND species IN ({species*}) 
     AND year BETWEEN {startYear} AND {endYear} 
     GROUP BY species, fishing_fleet, geom_id, geom_wkt, geom, year 
     ORDER BY species, fishing_fleet DESC",
    wkt = wkt, species_name = species, fishing_fleet_name = fleet,
    start_year = startYear, end_year = endYear,
    .con = pool
  )
  return(query)
}
