# mapCatchesmoduleslightUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     leafletOutput(ns("map_footprint"), width = "100%", height = "380px"),
#     actionButton(ns("submit_draw"), "Update WKT from drawing",
#                  class = "btn-primary",
#                  style = "position:absolute; top: 80px; right: 20px; z-index: 400; font-size: 0.8em; padding: 5px 10px;")
#   )
# }
# 
# # data(): reactive avec colonnes au moins geographic_identifier, measurement_value
# # geom_sf: sf (polygones de grille) avec colonne geographic_identifier
# # mode = "vector_dissolve" (par défaut) ou "raster"
# mapCatchesmoduleslightServer <- function(id, data, geom_sf, mode = reactive("vector_dissolve"), enabled = reactive(FALSE)) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     newwkt <- reactiveVal()
#     
#     # 1) Calcul empreinte (présence/absence) + jointure spatiale minimale
#     footprint_sf <- reactive({
#       req(enabled(), data())
#       ids <- data() |>
#         dplyr::group_by(geographic_identifier) |>
#         dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
#         dplyr::pull(geographic_identifier)
#       
#       # garde seulement les cellules présentes, puis DISSOUT en un seul MULTIPOLYGON
#       fp <- geom_sf |>
#         dplyr::semi_join(tibble::tibble(geographic_identifier = ids), by = "geographic_identifier")
#       
#       # Dissolve + simplification agressive pour la vitesse
#       fp_union <- sf::st_union(sf::st_make_valid(fp))
#       fp_union <- sf::st_simplify(fp_union, dTolerance = 0.05) # ajustez si besoin
#       sf::st_as_sf(tibble::tibble(geometry = fp_union)) |> sf::st_set_crs(sf::st_crs(geom_sf))
#     })
#     
#     # 2) Option raster ultra rapide (perte d’interactivité fine, mais super fluide)
#     footprint_raster <- reactive({
#       req(enabled(), footprint_sf())
#       # Crée un raster binaire très léger (1 = présence)
#       # Résolution volontairement grossière pour la vitesse; ajustez "dx"
#       dx <- 0.5
#       bbox <- sf::st_bbox(footprint_sf())
#       xs <- seq(bbox["xmin"], bbox["xmax"], by = dx)
#       ys <- seq(bbox["ymin"], bbox["ymax"], by = dx)
#       r <- stars::st_as_stars(stars::st_bbox(footprint_sf()), dx = dx, dy = dx)
#       r[] <- 0
#       r[stars::st_intersects(r, footprint_sf(), sparse = FALSE)] <- 1
#       r
#     })
#     
#     output$map_total_catch_light <- renderLeaflet({
#       req(enabled())
#       # Fond "continent" très léger via tuiles grises (rapide et lisible)
#       leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 1)) |>
#         addProviderTiles("CartoDB.PositronNoLabels") |>
#         clearBounds() |>
#         addDrawToolbar(targetGroup = "draw",
#                        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
#     })
#     
#     # 3) Peinture de l’empreinte selon le mode choisi
#     observe({
#       req(enabled())
#       m <- leafletProxy("map_total_catch_light", session = session) |> clearGroup("footprint") |> removeControl("legend_fp")
#       if (mode() == "raster") {
#         r <- footprint_raster()
#         m |>
#           addRasterImage(r, group = "footprint", opacity = 0.7) |>
#           addLayersControl(overlayGroups = "footprint", options = layersControlOptions(collapsed = TRUE))
#       } else {
#         fp <- footprint_sf()
#         m |>
#           addPolygons(data = fp,
#                       group = "footprint",
#                       fill = TRUE, fillOpacity = 0.6, weight = 1, color = "#444444",
#                       popup = NULL, label = NULL) |>
#           addLayersControl(overlayGroups = "footprint", options = layersControlOptions(collapsed = TRUE))
#       }
#     })
#     
#     # 4) Récupération WKT du dessin
#     observeEvent(input$submit_draw, {
#       gf <- input$map_total_catch_light_draw_new_feature
#       if (!is.null(gf) && !is.null(gf$geometry)) {
#         geojson_text <- jsonlite::toJSON(gf$geometry, auto_unbox = TRUE, pretty = FALSE)
#         sf_obj <- geojsonsf::geojson_sf(geojson_text)
#         newwkt(sf::st_as_text(sf_obj$geometry))
#       } else {
#         showModal(modalDialog("Please draw a polygon first.", easyClose = TRUE))
#       }
#     })
#     
#     return(list(newwkt = newwkt))
#   })
# }
