#' Update the logger info table
#'
#' This is a convenience function that writes to the "loggers.logger_info"
#'
#' @param loggerData A named vector or data frame that fits the logger_info table in schema loggers
#' @param append Logical, default True. If True, the line(s) is appended to the end of the table.
#' @param overwrite Logical, default False. WARNING!! If True, the function overwrites the current content of the logger_info table.
#'
#' @return Data frame.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' writeLoggerImport(sampleLoggerImport)
#'
#' }


writeLoggerImport <- function(loggerImport,
                              append = T,
                              overwrite = FALSE){
 checkCon()
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbSendQuery(con, "SET search_path TO imports, public")
      DBI::dbWriteTable(con, "logger_import", loggerImport, append = append, overwrite = overwrite)
    }
  )
}


