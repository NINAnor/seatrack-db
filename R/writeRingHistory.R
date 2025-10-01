#' Update the ring history table
#'
#' This is a convenience function that writes to the "individuals.ring_history".
#'
#' It is very important to store backups of this table outside the database! This is the memory of the history of the ring numbers and might need to be reimported if the database has been scratched!
#'
#'
#' @param historyData A named vector or data frame that fits the ring history table in schema individuals
#' @param append Logical, default True. If True, the line(s) is appended to the end of the table.
#' @param overwrite Logical, default False. WARNING!! If True, the function overwrites the current content of the logger_info table.
#'
#' @return Data frame.
#' @export
#' @examples
#' \dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' writeRingHistory(sampleRingHistory)
#' }


writeRingHistory <- function(historyData,
                             append = T){
  checkCon()
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbSendQuery(con, "SET search_path TO individuals, public")
      DBI::dbWriteTable(con, "ring_history",
                        historyData,
                        append = append)
    }
  )
}


