require(here)
source(here::here("materialized_view/executeSQLQuery.R"))
executeSQLQuery(con = con, queryFile = here::here("materialized_view/i6i7i8.sql"))
executeSQLQuery(con = con, queryFile = here::here("materialized_view/combined_catch_data.sql"))
