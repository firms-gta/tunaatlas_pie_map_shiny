-- Drop the existing materialized view if it exists
DROP MATERIALIZED VIEW IF EXISTS public.i6i7i8;

-- Recreate the materialized view
CREATE MATERIALIZED VIEW public.i6i7i8 AS
SELECT
  row_number() OVER () AS ogc_fid,
  "time".year AS year,
  species_labels.codesource_species AS species,
  fishing_fleet_labels.codesource_fishing_fleet AS fishing_fleet,
  sum(fact.measurement_value) AS measurement_value,
  count(fact.measurement_value) AS count,
  area.codesource_area,
  cwp_grid.geom,fact.id_area AS geom_id
FROM fact_tables.catch fact 
LEFT JOIN "time"."time" USING (id_time)
LEFT JOIN species.species_labels USING (id_species)
LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = fact.id_species
LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to 
LEFT JOIN fishing_fleet.fishing_fleet_labels USING (id_fishing_fleet)
LEFT JOIN fishing_fleet.fishing_fleet_mapping ON fishing_fleet_mapping.fishing_fleet_mapping_id_from = fact.id_fishing_fleet
LEFT JOIN fishing_fleet.fishing_fleet_labels fishing_fleetgroup_label ON fishing_fleetgroup_label.id_fishing_fleet = fishing_fleet_mapping.fishing_fleet_mapping_id_to 
LEFT JOIN area.area USING (id_area)
LEFT JOIN area.cwp_grid ON area.cwp_grid.code = area.codesource_area
WHERE fact.id_metadata = 101 AND cwp_grid.gridtype = '5deg_x_5deg'
GROUP BY fact.id_area, area.codesource_area, "time".year, species_labels.codesource_species, fishing_fleet_labels.codesource_fishing_fleet,cwp_grid.geom;

-- Refresh the materialized view
REFRESH MATERIALIZED VIEW public.i6i7i8;

-- Grant necessary privileges to the materialized view
ALTER TABLE public.i6i7i8 OWNER TO tunaatlas_u;
GRANT ALL ON TABLE public.i6i7i8 TO tunaatlas_u;
GRANT SELECT ON TABLE public.i6i7i8 TO tunaatlas_inv;
