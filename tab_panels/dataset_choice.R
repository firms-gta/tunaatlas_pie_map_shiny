dataset_choice <- function(){
  nav_panel("Changement de Dataset/Gridtype",
            useShinyjs(),
            grid_container(
              layout = c(
                "choosedatagrid  explenation",
                "choosedatagrid explenation"
              ),   
              row_sizes = c(
                "1.48fr",
                "0.52fr"
              ),
              col_sizes = c(
                "1.64fr",
                "0.36fr"
              ),
              gap_size = "10px",
              # grid_card_plot(area = "plot"),
              grid_card_text(
                content = "Explenation",
                alignment = "start",
                area = "explenation"
              ),
              grid_card(
                area = "choosedatagrid",
                card_body(uiOutput("select_dataset"),     
                          tags$br(),
                          uiOutput("select_gridtype"),
                          actionButton("submit_dataset", "Submit Dataset Selection"))
              )
           )
         )
}
