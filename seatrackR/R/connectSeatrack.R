#' Connect to seatrack database
#'
#' This function establishes a connection to the Seatrack database. It is a simple convenience function that uses the
#' packages DBI and RPostgres (available from github via devtools::install_github("rstats-db/DBI") and
#' devtools::install_github("rstats-db/DBI"). Note that connections are only accepted from a limited IP-adresses.
#'
#' @param Username Character. Default = seatrack_reader
#' @param Password Character.
#' @param host Character. The host of the database. For testing purposes. There should be no need for the user to change this.
#' @param dbname Character. Name of database, for testing purposes. Default is "seatrack" which is the production database.
#' @param check_interrupts True/False. Should user interrupts be checked during the query execution (before
#'   first row of data is available)? Setting to `TRUE` allows interruption of queries
#'   running too long.
#' @return A DBI connection to the Seatrack database
#' @import DBI
#' @export
#'
#' @note The password is stored within the R session in a somewhat hidden environment, to be used in the interface with the FTP server.
#' It is therefore not saved in the session if you do that when closing R. The password is not stored in cleartext within the database either, and the admins have no way of seeing it.
#' The handshake between R and the FTP server is encrypted, as well as the actual data transfers.
#' However, it is difficult to guarantee that it is totally safe from all eventualities.
#' For example, I don't know what would happen if R would crash and store something in a crash logfile. So best practice would be to use a separate password for
#' Seatrack, that you don't share with other sites or applications.
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' DBI::dbGetQuery(con, "SELECT * FROM loggers.logging_session LIMIT 10")
#' DBI::dbDisconnect(con)
#' }

connectSeatrack <- function(Username = "testreader",
                            Password = "testreader",
                            host = "seatrack.nina.no",
                            dbname = "seatrack",
                            bigint = "integer",
                            ...) {


  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it using devtools::install_github(\"rstats-db/DBI\") ",
         call. = FALSE)
  }


  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it using devtools::install_github(\"rstats-db/RPostgres\") ",
         call. = FALSE)
  }


  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        user = Username,
                        password = Password,
                        ...)

  assign("con", tmp, .GlobalEnv)
  assign(".pass", Password, envir = passEnv)

#Set the timezone to correspond to the database timezone
Sys.setenv(TZ = "Europe/Oslo")


}

#' @export
disconnectSeatrack <- function(){
  DBI::dbDisconnect(con)
}

