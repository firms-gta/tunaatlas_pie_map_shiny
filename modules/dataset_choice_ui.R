dataset_choice_ui <- function(id) {
  ns <- NS(id)
  nav_panel(title = "Choosing dataset and gridtype", value = "datasetchoicevalue",
            grid_container(
              layout = c(
                "choosedatagrid  explenation",
                "submitbutton ."
              ),
              row_sizes = c(
                "1.5fr",
                "0.5fr"
              ),
              col_sizes = c(
                "0.36fr",
                "1.64fr"
              ),
              gap_size = "10px",
              grid_card(area = "explenation",
                        card_body(tags$iframe(src = "www/Datasets.html", width = "100%", 
                                              height = "100%"))
              ),
              grid_card(
                area = "choosedatagrid",
                card_body(uiOutput(ns("select_dataset")),     
                          tags$br(),
                          uiOutput(ns("select_gridtype")), 
                          uiOutput(ns("select_measurement_unit"))
                )
              ),
              grid_card(area = "submitbutton",
                        card_body(actionButton(ns("submitDataset"), "Submit Dataset"))
              )
            )
  )
}


dataset_choice_server <- function(id, filters_combinations, default_dataset, default_gridtype, default_measurement_unit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$select_dataset <- renderUI({
      datasets <- filters_combinations %>% dplyr::select(dataset) %>% dplyr::distinct()
      selectizeInput(ns('select_dataset'), 'Select the Dataset', choices = datasets$dataset, selected = default_dataset)
    })
    
    output$select_gridtype <- renderUI({
      req(input$select_dataset)
      gridtypes <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% dplyr::select(gridtype) %>% dplyr::distinct()
      selectizeInput(ns('select_gridtype'), 'Select the Grid Type', choices = gridtypes$gridtype, selected = default_gridtype)
    })
    
    output$select_measurement_unit <- renderUI({
      req(input$select_dataset)
      measurement_units <- filters_combinations %>% dplyr::filter(dataset == input$select_dataset) %>% dplyr::select(measurement_unit) %>% dplyr::distinct()
      selectizeInput(ns('select_measurement_unit'), 'Select the Measurement Unit', choices = measurement_units$measurement_unit, selected = default_measurement_unit)
    })
    
    return(list(
      selected_dataset = reactive(input$select_dataset),
      selected_gridtype = reactive(input$select_gridtype),
      selected_measurement_unit = reactive(input$select_measurement_unit),
      submit = reactive(input$submitDataset)
    ))
  })
}

