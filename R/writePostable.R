#' Update the positions.postable
#'
#' This is a convenience function that writes to the "positions.postable" table, the main table for the position data.
#'
#' @param positionData A list of position data to be read into the postable in the database. Usually created by `loadPosdata`.
#' @return Data frame.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#'
#'  files<-c("posdata_FULGLA_eynhallow_2014",
#'           "posdata_FULGLA_eynhallow_2013",
#'           "posdata_FULGLA_eynhallow_2012",
#'           "posdata_FULGLA_eynhallow_2011",
#'           "posdata_FULGLA_eynhallow_2010",
#'           "posdata_FULGLA_eynhallow_2009",
#'           "posdata_FULGLA_eynhallow_2007"
#'           )
#'
#' toImport <- loadPosdata(files)
#'
#' summary(toImport)
#'
#' writePostable(toImport)
#'
#' }


writePostable <- function(positionData){
  seatrackR:::checkCon()

  nRowsToImport <- sum(unlist(lapply(positionData, nrow)))

  # nRowsBefore <- DBI::dbWithTransaction(
  #   con,
  #   {
  #     DBI::dbGetQuery(con, "SELECT count(*) FROM positions.postable")
  #   }
  # )

  DBI::dbWithTransaction(
    con,
    {
      nRowsBefore <- DBI::dbGetQuery(con, "SELECT count(*) FROM positions.postable")
      DBI::dbSendQuery(con, "SET search_path TO positions, public")

      for(i in 1:length(positionData)){
        dbWriteTable(con, "postable", positionData[[i]], row.names=F, append=T)
      }


      nRowsAfter <- DBI::dbGetQuery(con, "SELECT count(*) FROM positions.postable")

      nRowsImported <- nRowsAfter - nRowsBefore

      if(nRowsImported != nRowsToImport){
        dbBreak()
        return("Not all lines could be imported, aborted import!")
      }

    })


      return(paste0("All ", nRowsToImport, " lines imported."))


}


