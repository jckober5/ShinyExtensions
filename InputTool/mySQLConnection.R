library(RMySQL)
library(DBI)

# Function to establish database connection
dbcon <- function() {
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        #dbname = "forms",
                        host = "23.239.4.168",
                        port = 3306,
                        user = "formUser",
                        password = "n6rWGN92e!"
  )
  return(con)
}