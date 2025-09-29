#' Retrieve info on the ring history table
#'
#' Read from the table of the history of ring numbers for birds that have had their rings changed.
#'
#'
#' @return Lazy query, or optionally a tibble
#' @export
#' @examples
#' dontrun{
#' getRingHistory()
#' }


getRingHistory <- function(asTibble = F){
  seatrackR:::checkCon()

  res <- dplyr::tbl(con, dbplyr::in_schema("individuals", "ring_history"))  %>% select(-id)

  if(asTibble){
    res <- res  %>% dplyr::collect()
  }

  return(res)
}





