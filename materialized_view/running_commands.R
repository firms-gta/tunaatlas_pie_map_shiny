require(here)
source(here::here("materialized_view/executeSQLQuery.R"))
# executeSQLQuery(con = pool, queryFile = here::here("materialized_view/i6i7i8.sql"))
executeSQLQuery(con = pool, queryFile = here::here("materialized_view/combined_catch_data.sql"))
