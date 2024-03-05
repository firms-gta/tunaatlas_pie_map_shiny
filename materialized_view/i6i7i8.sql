-- Drop the existing materialized view if it exists
DROP MATERIALIZED VIEW IF EXISTS public.i6i7i8;

-- Recreate the materialized view
CREATE MATERIALIZED VIEW public.i6i7i8 AS
SELECT
  row_number() OVER () AS ogc_fid,
  "time".year,
  species_labels.codesource_species AS species,
  fishing_fleet_labels.codesource_fishing_fleet AS fishing_fleet,
  sum(fact.measurement_value) AS measurement_value,
  count(fact.measurement_value) AS count,
  area.codesource_area,
  cwp_grid.geom,
  fact.id_area AS geom_id,
  cwp_grid.gridtype as gridtype
FROM fact_tables.catch fact
LEFT JOIN metadata.metadata ON metadata.id_metadata = fact.id_metadata AND metadata.identifier = 'global_catch_firms_level0_' 
LEFT JOIN "time"."time" ON "time".id_time = fact.id_time
LEFT JOIN species.species_labels ON species.species_labels.id_species = fact.id_species
LEFT JOIN species.species_mapping_view ON species.species_mapping_view.db_idsource = fact.id_species
LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species.species_mapping_view.db_idtarget
LEFT JOIN fishing_fleet.fishing_fleet_labels ON fishing_fleet.fishing_fleet_labels.id_fishing_fleet = fact.id_fishing_fleet
LEFT JOIN fishing_fleet.fishing_fleet_mapping_view ON fishing_fleet.fishing_fleet_mapping_view.db_idsource = fact.id_fishing_fleet
LEFT JOIN fishing_fleet.fishing_fleet_labels fishing_fleetgroup_label ON fishing_fleetgroup_label.id_fishing_fleet = fishing_fleet.fishing_fleet_mapping_view.db_idtarget
LEFT JOIN area.area ON area.area.id_area = fact.id_area
LEFT JOIN area.cwp_grid ON area.cwp_grid.code = area.area.codesource_area
GROUP BY fact.id_area, area.codesource_area, "time".year, species_labels.codesource_species, fishing_fleet_labels.codesource_fishing_fleet, cwp_grid.geom,cwp_grid.gridtype;


