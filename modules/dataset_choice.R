# deprecated
# dataset_choice_ui <- function(id) {
#   ns <- NS(id)
#   nav_panel(
#     title = "Choosing Dataset and Source",
#     value = "datasetchoicevalue",
#     
#     grid_container(
#       layout = c(
#         "choosesourcetype explanation",
#         "subpanels explanation",
#         "submitbutton ."
#       ),
#       row_sizes = c(
#         "1fr",
#         "2fr",
#         "0.5fr"
#       ),
#       col_sizes = c(
#         "0.4fr",
#         "1.6fr"
#       ),
#       gap_size = "10px",
#       
#       grid_card(area = "explanation",
#                 card_body(tags$iframe(src = "www/Datasets.html", width = "100%", height = "100%"))
#       ),
#       
#       grid_card(
#         area = "choosesourcetype",
#         card_body(
#           radioButtons(ns("source_type"), "Choose data source:", 
#                        choices = c("Database" = "db", "Workspace/DOI" = "doi"), 
#                        selected = "doi")
#         )
#       ),
#       
#       grid_card(
#         area = "subpanels",
#         card_body(
#           conditionalPanel(
#             condition = paste0("input['", ns("source_type"), "'] == 'doi'"),
#             uiOutput(ns("doi_panel"))
#           ),
#           conditionalPanel(
#             condition = paste0("input['", ns("source_type"), "'] == 'db'"),
#             uiOutput(ns("db_panel"))
#           )
#         )
#       ),
#       
#       grid_card(area = "submitbutton",
#                 card_body(actionButton(ns("submitDataset"), "Submit Dataset"))
#       )
#     )
#   )
# }
# 
# 
# # Server Module
# dataset_choice_server <- function(id, filters_combinations, default_dataset, default_gridtype, default_measurement_unit) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Charger les données DOI une fois
#     doi_data <- tryCatch(
#       read_csv("DOI.csv", show_col_types = FALSE),
#       error = function(e) {
#         flog.error("Erreur lors de la lecture du fichier DOI.csv : %s", e$message)
#         data.frame(Filename = character())
#       }
#     )
#     
#     if (nrow(doi_data) == 0 || !"Filename" %in% colnames(doi_data)) {
#       stop("Le fichier DOI.csv est invalide ou vide.")
#     }
#     
#     # Panel DOI
#     output$doi_panel <- renderUI({
#       tagList(
#         selectizeInput(
#           ns("select_doi_dataset"),
#           "Select Dataset from DOI:",
#           choices = doi_data$Filename,
#           selected = doi_data$Filename[1]
#         )
#       )
#     })
#     
#     # Panel Database
#     output$db_panel <- renderUI({
#       if (is.null(filters_combinations()) || nrow(filters_combinations()) == 0) {
#         return(tags$p("No connection to DB", style = "color: red;"))
#       }
#       tagList(
#         uiOutput(ns("select_dataset")),
#         tags$br(),
#         uiOutput(ns("select_gridtype")),
#         uiOutput(ns("select_measurement_unit"))
#       )
#     })
#     
#     # UI pour sélectionner un dataset
#     output$select_dataset <- renderUI({
#       datasets <- filters_combinations() %>% dplyr::select(dataset) %>% dplyr::distinct()
#       selectizeInput(
#         ns("select_dataset"),
#         "Select the Dataset",
#         choices = datasets$dataset,
#         selected = ifelse(nrow(datasets) > 0, default_dataset, NULL)
#       )
#     })
#     
#     # UI pour sélectionner un grid type
#     output$select_gridtype <- renderUI({
#       req(filters_combinations(), input$select_dataset)
#       gridtypes <- filters_combinations() %>%
#         dplyr::filter(dataset == input$select_dataset) %>%
#         dplyr::select(gridtype) %>%
#         dplyr::distinct()
#       selectizeInput(
#         ns("select_gridtype"),
#         "Select the Grid Type",
#         choices = gridtypes$gridtype,
#         selected = ifelse(nrow(gridtypes) > 0, default_gridtype, NULL),
#         multiple = TRUE
#       )
#     })
#     
#     # UI pour sélectionner une unité de mesure
#     output$select_measurement_unit <- renderUI({
#       req(filters_combinations(), input$select_dataset)
#       measurement_units <- filters_combinations() %>%
#         dplyr::filter(dataset == input$select_dataset) %>%
#         dplyr::select(measurement_unit) %>%
#         dplyr::distinct()
#       selectizeInput(
#         ns("select_measurement_unit"),
#         "Select the Measurement Unit",
#         choices = measurement_units$measurement_unit,
#         selected = ifelse(nrow(measurement_units) > 0, default_measurement_unit, NULL),
#         multiple = TRUE
#       )
#     })
#     
#     # Retourner les valeurs sélectionnées
#     return(list(
#       selected_source = reactive(input$source_type),
#       selected_dataset = reactive({
#         if (input$source_type == "db") {
#           input$select_dataset
#         } else {
#           input$select_doi_dataset
#         }
#       }),
#       selected_gridtype = reactive(input$select_gridtype),
#       selected_measurement_unit = reactive(input$select_measurement_unit),
#       submit = reactive(input$submitDataset)
#     ))
#   })
# }
