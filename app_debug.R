
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

global_catch_5deg_1m_firms_level0_public_debug <- global_catch_5deg_1m_firms_level0_public %>% head(1000)

# Lancer l'application avec le paramètre debug
shinyApp(
  ui = ui,
  server = function(input, output, session) {
    server(input, output, session)
  }
)

