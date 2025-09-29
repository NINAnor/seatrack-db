#' View the database structure in a browser
#'
#' Zoom in with the browser to see the details
#'
#'
#' @return Opens a internet browser window with the database model.
#' @export
#' @examples
#' dontrun{
#' viewDatabaseModel()
#' }
#'

viewDatabaseModel <- function(type = c("png", "svg")){
  type <- match.arg(type, c("png", "svg"))

  browseURL(system.file("img", paste0("seatrackModel.", type), package = "seatrackR"))
}
