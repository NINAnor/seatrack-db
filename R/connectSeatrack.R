#' Connect to seatrack database
#'
#' This function establishes a connection to the Seatrack database. Note that connections are only accepted from limited IP-adresses.
#' Ideally, credentials should first be set using `set_credentials_renviron()`. This should only have to be done once per project.
#' After this, credentials will be loaded automatically.
#'
#' The function opens a connection to the database, which other functions in seatrackRdb will use.
#' If a user wants to access the connection in their own R environment so as to write their own queries, they can do so using `getSeatrackConnection()`
#'
#' @param Username Character. If not provided, first attempts to check environmenta variables then calls set_credentials_renviron()
#' @param Password Character. If not provided, first attempts to check environmenta variables then calls set_credentials_renviron()
#' @param host Character. The host of the database. For testing purposes. There should be no need for the user to change this.
#' @param dbname Character. Name of database, for testing purposes. Default is "seatrack" which is the production database.
#' @return No return, a connection to the database is opened
#' @export
#' @examples
#' \dontrun{
#' connectSeatrack()
#' con <- getSeatrackConnection()
#' DBI::dbGetQuery(con, "SELECT * FROM loggers.logging_session LIMIT 10")
#' DBI::dbDisconnect(con)
#' }

connectSeatrack <- function(user_name = NULL,
                            password = NULL,
                            host = "seatrack.nina.no",
                            dbname = "seatrack",
                            ...) {


  if(is.null(user_name)){
    user_name <- Sys.getenv("SEATRACK_DB_USER", NULL)
  }
  if(is.null(password)){
    password <- Sys.getenv("SEATRACK_DB_USER", NULL)
  }
  if(is.null(user_name) || is.null(password)){
    set_credentials_renviron()
  }

  # This sets the server time as UTC and the import time as UTC.
  # This will return the time zone non-aware UTC times correctly.
  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        user = Username,
                        password = Password,
                        ...)

  assign("con", tmp, the)

}

#' @export
disconnectSeatrack <- function(){
  DBI::dbDisconnect(the$con)
}

#' Return ongoing database connection
#'
#' Utility function to access the seatrack db connection
#' @return DBI connection object
#' @export
getSeatrackConnection <- function() {
  if (!DBI::dbIsValid(the$con)) {
    stop("No connection, run connectSeatrack()")
  } else {
    return(the$con)
  }
}
