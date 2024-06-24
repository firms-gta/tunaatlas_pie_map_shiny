dataset_choice <- function(){
  nav_panel(title = "Choosing dataset and gridtype", value = "datasetchoicevalue",
            grid_container(
              layout = c(
                "choosedatagrid  explenation",
                "submitbutton ."
              ),
              row_sizes = c(
                "1.5fr",
                "0.5fr",
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
                card_body(uiOutput("select_dataset"),     
                          tags$br(),
                          uiOutput("select_gridtype"), 
                          uiOutput("select_measurement_unit")
                )
              ),
              grid_card(area = "submitbutton",
                        card_body(actionButton("submitDataset", "Submit Dataset"))
              )
            )
  )
}
