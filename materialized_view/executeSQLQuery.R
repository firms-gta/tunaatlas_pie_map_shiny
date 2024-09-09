executeSQLQuery <- function(con, queryFile) {
  require(DBI)
  require(RSQLite)
  # Read the SQL query from the file
  query <- readLines(queryFile)
  
  # Remove leading/trailing whitespace and join the lines into a single string
  query <- paste(trimws(query), collapse = " ")
  
  # Establish a database connection
  # Replace the connection details with your own
  
  # Execute the SQL query
  result <- DBI::dbExecute(con, query)
  
  # Close the database connection
  DBI::dbDisconnect(con)
  
  # Return the query result
  return(result)
}
