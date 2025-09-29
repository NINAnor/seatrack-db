#' changeSeatrackPassword
#'
#' Changes the password for a user in the Seatrack database. Since the passwords for the file archive are fetched from the database,
#' this also affects the file archive.
#'
#'
#' @param password Your new password (character string).
#'
#' @return Null
#' @export
#' @examples
#' \dontrun{
#' changeSeatrackPassword("newPassword")
#' }
#'

changeSeatrackPassword <- function(password = NULL){

  checkCon()

  current_user <- DBI::dbGetQuery(con, "SELECT current_user")

  alterQuery <- paste0("ALTER USER ", current_user, " WITH PASSWORD '", password, "';")

  mess <- DBI::dbExecute(con, alterQuery)

  disconnectSeatrack()

  return("Password changed, you need to connect again using connectSeatrack()")

}
