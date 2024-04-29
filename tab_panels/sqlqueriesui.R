sqlqueriesui <- function(){
  tabPanel("SQL Queries",
         div(class="outer",
             h3("SQL Queries Overview"),
             p("Here you can find the SQL queries used to retrieve the data displayed in this application."),
             title = "Your SQL query for overview",
             textOutput("sql_query_metadata"),
             title = "Your SQL query",
             textOutput("sql_query")
             
             
         )
)
}