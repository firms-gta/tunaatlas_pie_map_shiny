CREATE MATERIALIZED VIEW public.combined_catch_data
TABLESPACE pg_default
AS
SELECT 
row_number() OVER () AS ogc_fid,
metadata.identifier AS dataset,
"time".year,
geargoup_label.codesource_gear_type AS gear_group,
species_labels.codesource_species AS species,
fishing_fleet_labels.codesource_fishing_fleet AS fishing_fleet,
sum(fact.measurement_value) AS measurement_value,
count(fact.measurement_value) AS count_values,
measurement_unit_labels.codesource_measurement_unit AS measurement_unit,
area.codesource_area,
cwp_grid.geom,
st_area(area_labels.geom) AS area, 
fact.id_area AS geom_id
FROM metadata.metadata fact_tables.catch fact
LEFT JOIN measurement_unit.measurement_unit_labels USING (id_measurement_unit)
LEFT JOIN gear_type.gear_type_labels USING (id_gear_type)
LEFT JOIN gear_type.gear_type_mapping ON gear_type_mapping.gear_type_mapping_id_from = tab.id_gear_type
LEFT JOIN gear_type.gear_type_labels geargoup_label ON geargoup_label.id_gear_type = gear_type_mapping.gear_type_mapping_id_to
LEFT JOIN "time"."time" USING (id_time)
LEFT JOIN species.species_labels species_labels ON fact.id_species = species_labels.id_species
LEFT JOIN fishing_fleet.fishing_fleet_labels fishing_fleet_labels ON fact.id_fishing_fleet = fishing_fleet_labels.id_fishing_fleet
LEFT JOIN measurement_unit.measurement_unit_labels measurement_unit_labels ON fact.id_measurement_unit = measurement_unit_labels.id_measurement_unit
LEFT JOIN area.area ON fact.id_area = area.id_area
LEFT JOIN area.cwp_grid ON cwp_grid.code = area.codesource_area
LEFT JOIN area.area_labels al ON fact.id_area = al.id_area
WHERE 
metadata.identifier = ANY(ARRAY['global_catch_firms_level0', 'global_catch_5deg_1m_firms_level0',
                         /* 'global_catch_1deg_1m_ps_bb_firms_level0', 'global_catch_1deg_1m_ps_bb_ird_level1',
                          'global_catch_1deg_1m_ps_bb_ird_level2', 'global_catch_5deg_1m_ird_level1',
                          'global_catch_5deg_1m_ird_level2', 'global_catch_ird_level1','global_catch_5deg_1m_firms_level1',  */ 
                         'global_catch_ird_level2'])
GROUP BY 
metadata.identifier, fact.id_area, area.codesource_area, "time".year, species_labels.codesource_species, fishing_fleet_labels.codesource_fishing_fleet, cwp_grid.geom,gear_type.gear_type_labels, geargoup_label, measurement_unit_labels.codesource_measurement_unit 
WITH DATA;

ALTER TABLE public.combined_catch_data
OWNER TO tunaatlas_u;

GRANT ALL ON TABLE public.combined_catch_data TO tunaatlas_u;
GRANT SELECT ON TABLE public.combined_catch_data TO tunaatlas_inv;
