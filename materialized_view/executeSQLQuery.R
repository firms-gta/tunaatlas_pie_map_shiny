executeSQLQuery <- function(queryFile) {
  # Read the SQL query from the file
  query <- readLines(queryFile)
  
  # Remove leading/trailing whitespace and join the lines into a single string
  query <- paste(trimws(query), collapse = " ")
  
  # Establish a database connection
  # Replace the connection details with your own
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = "path_to_your_database.db"
  )
  
  # Execute the SQL query
  result <- DBI::dbGetQuery(con, query)
  
  # Close the database connection
  DBI::dbDisconnect(con)
  
  # Return the query result
  return(result)
}
