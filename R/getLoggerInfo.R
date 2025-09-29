#' View the view info table
#'
#' This is a convenience function that reads from the view "views.logger_info". Note that there also exists a table "loggers.logger_info" with more limited information.
#' #'
#' @param asTibble Boolean. Return result as Tibble instead of Lazy query? Tibble is slower, but also here forces the timezone to "UTC".
#'
#' @return Lazy query or optionally a Tibble.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' loggerInfo <- getLoggerInfo()
#' }

getLoggerInfo <- function(asTibble = T){
  seatrackR:::checkCon()

  res <-dplyr::tbl(con, dbplyr::in_schema("views", "logger_info"))

  if(asTibble){
    res <- res  %>% dplyr::collect()

    #Forze timezone to be UTC
    res <- res %>%
      mutate(starttime_gmt = lubridate::force_tz(starttime_gmt,
                                                 tzone = "UTC"),
             programmed_gmt_time = lubridate::force_tz(programmed_gmt_time,
                                                       tzone = "UTC"),
      )
  }



  return(res)

}




