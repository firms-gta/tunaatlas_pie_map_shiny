# 
# initialize_data_and_plots <- function(data_init, pool, sql_query_init, output_dir) {
#   if (file.exists(file.path(output_dir, "map_init.html")) &&
#       file.exists(here::here("tab_panels/plot_init.png"))) {
#     flog.info("Map and plot already exist. Skipping initialization.")
#     return(NULL)
#   }
#   
#   flog.info("Initializing data and plots...")
#   
  # data_without_geom <- as.data.frame(data_init)
  # data_without_geom$geom_wkt <- NULL
  # df <- data_without_geom %>%
  #   dplyr::group_by(.data[["fishing_fleet"]], year) %>%
  #   dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop") %>%
  #   ungroup()
  # 
  # top_n_groups <- df %>%
  #   dplyr::group_by(.data[["fishing_fleet"]]) %>%
  #   dplyr::summarise(total = sum(measurement_value)) %>%
  #   dplyr::top_n(5, total) %>%
  #   pull(.data[["fishing_fleet"]])
  # 
  # df <- df %>%
  #   dplyr::mutate(!!sym("fishing_fleet") := if_else(.data[["fishing_fleet"]] %in% top_n_groups, as.character(.data[["fishing_fleet"]]), "Other")) %>%
  #   dplyr::group_by(.data[["fishing_fleet"]], year) %>%
  #   dplyr::summarise(measurement_value = sum(measurement_value), .groups = "drop")
  # 
  # plot_init_by_dim_overview <- ggplot(df, aes_string(x = "year", y = "measurement_value", group = "fishing_fleet", color = "fishing_fleet")) +
  #   geom_line() + labs(title = "Yearly Data", x = "Year", y = "Measurement Value")
  # 
  # png(here::here("tab_panels/plot_init.png"))
  # print(plot_init_by_dim_overview)
  # dev.off()
#   
#   a <- st_read(pool, query = paste0("SELECT geom, sum(measurement_value) AS measurement_value FROM(", sql_query_init, ") AS foo GROUP BY geom"))
#   
#   qpal <- colorQuantile(rev(viridis::viridis(10)), a$measurement_value, n = 10)
#   tmap_mode("plot")
#   
#   map <- leaflet() %>%
#     addProviderTiles("Esri.NatGeoWorldMap") %>%
#     clearBounds() %>%
#     addPolygons(data = a,
#                 label = ~measurement_value,
#                 popup = ~paste0("Total catches for the selected criteria in this square of the grid: ", round(measurement_value), " tons (t) et des brouettes"),
#                 fillColor = ~qpal(measurement_value),
#                 fill = TRUE,
#                 fillOpacity = 0.8,
#                 smoothFactor = 0.5) %>%
#     addDrawToolbar(
#       targetGroup = "draw",
#       editOptions = editToolbarOptions(
#         selectedPathOptions = selectedPathOptions()
#       )
#     ) %>%
#     addLayersControl(
#       overlayGroups = c("draw"),
#       options = layersControlOptions(collapsed = FALSE)
#     ) %>%
#     leaflet::addLegend("bottomright", pal = qpal, values = a$measurement_value,
#                        title = "Quantile of the grid for the total catches",
#                        labFormat = labelFormat(prefix = "MT "),
#                        opacity = 1)
#   
#   htmlwidgets::saveWidget(map, file.path(output_dir, "map_init.html"))
#   
#   flog.info("Data and plots initialized successfully.")
#   
#   return(map)
# }
# 
# # Spécifiez le répertoire de sortie pour les fichiers HTML
# output_dir <- "www"
# dir.create(output_dir, showWarnings = FALSE)
# 
# # Vérifiez si tous les fichiers nécessaires existent
# sql_query_file <- file.path("tab_panels", "sql_query_init.rds")
# data_init_file <- file.path("data", "datasf.rds")
# files_exist <- file.exists(file.path(output_dir, "map_init.html")) &&
#   file.exists(here::here("tab_panels/plot_init.png")) &&
#   file.exists(sql_query_file) &&
#   file.exists(data_init_file)

# if (!files_exist) {
#   # sql_query_init <- createSQLQuery(limit = 1000)
#   
#   flog.info("Reading big data")
#   default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
#   default_dataset_preloaded_without_geom <- default_dataset_preloaded
#   default_dataset_preloaded_without_geom$geom <- NULL
#   default_dataset_preloaded_without_geom$geom_wkt <- NULL
#   flog.info("Finished Reading big data")
#   
#   # Sauvegarder sql_query_init et data_init
#   # saveRDS(sql_query_init, sql_query_file)
#   # saveRDS(data_init, data_init_file)
#   
#   map_init <- initialize_data_and_plots(data_init, pool, sql_query_init, output_dir)
# } else {

# flog.info("All initialization files already exist. Loading from files.")
# flog.info("loading inital data")
# 
# sql_query_init <- readRDS(here::here("tab_panels/sql_query_init.rds"))
# flog.info("SQl query loaded")
# 
# default_dataset_preloaded <- readRDS(here::here("data/datasf.rds"))
# flog.info("Data sf loaded")
# 
# map_init <- read_html(here::here("www/map_init.html"))
# flog.info("Map init loaded")

# }
