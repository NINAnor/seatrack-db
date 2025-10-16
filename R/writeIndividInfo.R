#' Update the individ_info table
#'
#' This is a convenience function that writes to the "individuals.individ_info"
#'
#' @param loggerData A named vector or data frame that fits the individ_info table in schema individuals
#' @param append Logical, default True. If True, the line(s) is appended to the end of the table.
#' @param overwrite Logical, default False. WARNING!! If True, the function overwrites the current content of the logger_info table.
#'
#' @return Data frame.
#' @export
#' @examples
#' \dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' writeIndividInfo(sampleIndividInfo)
#' }


writeIndividInfo <- function(individData,
                             append = T,
                             overwrite = FALSE){
  checkCon()
  DBI::dbWithTransaction(
    the$con,
    {
      DBI::dbSendQuery(the$con, "SET search_path TO individuals, public")
      DBI::dbWriteTable(the$con, "individ_info", individData, append = append, overwrite = overwrite)
    }
  )
}


