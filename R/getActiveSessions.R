#' View the active logger sessions
#'
#' This is a convenience function that reads from the view "views.active_logging_sessions. It simply lists all logging
#' sessions marked "active = TRUE"
#'
#' @return A tibble.
#' @import dplyr
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' activeSessions <- getActiveSessions()
#' }

getActiveSessions <- function() {
  checkCon()

  DBI::dbGetQuery(con, "SELECT * FROM views.active_logging_sessions")  %>%
    as_tibble()
}

