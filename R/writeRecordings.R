#' Update light, temperature or activity data
#'
#' This is a convenience function that writes to the light, temperature or the activity table in schema Recordings
#'
#' @param lightData A named vector or data frame that fits the light table in schema recordings
#' @param activityData A named vector or data frame that fits the activity table in schema recordings
#' @param temperatureData A named vector or data frame that fits the temperature table in schema recordings
#' @param append Logical, default True. If True, the line(s) is appended to the end of the table.
#' @param overwrite Logical, default False. WARNING!! If True, the function overwrites the current content of the logger_info table.
#'
#' @return Data frame.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' writeRecordings(lightData = sampleLightData)
#' }


writeRecordings <- function(lightData = NULL,
                            activityData = NULL,
                            temperatureData = NULL,
                            append = T,
                            overwrite = FALSE){
  checkCon()

  if(!is.null(lightData)){
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbWriteTable(con, Id(schema = "recordings", table = "light"), lightData, append = append, overwrite = overwrite)
    }
  )
  }

  if(!is.null(activityData)){
    DBI::dbWithTransaction(
      con,
      {
        DBI::dbWriteTable(con, Id(schema = "recordings", table = "activity"), activityData, append = append, overwrite = overwrite)
      }
    )
  }

  if(!is.null(temperatureData)){
    DBI::dbWithTransaction(
      con,
      {
        DBI::dbWriteTable(con, Id(schema = "recordings", table = "temperature"), temperatureData, append = append, overwrite = overwrite)
      }
    )
  }

}


