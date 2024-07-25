
# Connexion à la base de données
pool <- dbPool(
  RPostgreSQL::PostgreSQL(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user_readonly,
  password = db_password
)

# Charger le serveur et l'UI
source("server.R")
source("ui.R")

# Lancer l'application avec le paramètre debug
shinyApp(
  ui = ui,
  server = function(input, output, session) {
    server(input, output, session, debug = TRUE, default_dataset_preloaded = global_catch_firms_level0_public %>% head(10000))
  }
)

