#' Delete rows from positions.postable
#'
#' This is a convenience function that deletes rows from the "positions.postable" table, specified by which posdata file they originated from.
#' USE THIS WITH CAUTION!
#'
#' @param delFiles A character vector of posdata files to delete from the postable.
#'
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' delFiles<-c("posdata_FULGLA_eynhallow_2015")
#' deletePostable(delFiles)
#'
#' }


deletePostable <- function(delFiles){
  seatrackR:::checkCon()

  filesInPostable <- list()
    for(i in 1:length(delFiles)){

    toDelete <- paste0("SELECT posdata_file
                     FROM positions.postable
                     WHERE posdata_file = '",
      delFiles[i],
      "'
      LIMIT 1")

    result <- dbGetQuery(con, toDelete)
    filesInPostable <- c(filesInPostable, result)
  }


  sumFilesExisting <- sum(unlist(lapply(filesInPostable, function(x) length(x) > 0)))

  if(length(delFiles) != sumFilesExisting) stop(paste0(files[!(files %in% unlist(filesInPostable))], " not in postable!"))

  rowsInPostable <- list()
  for(i in 1:length(delFiles)){

    toDelete <- paste0("SELECT count(*)::integer
                     FROM positions.postable
                     WHERE posdata_file = '",
                       delFiles[i],
                       "'
      LIMIT 1")

    result <- dbGetQuery(con, toDelete)
    rowsInPostable <- c(rowsInPostable, result)
  }

  nRowsToDelete <- sum(unlist(rowsInPostable))


  DBI::dbWithTransaction(
    con,
    {
      nRowsBefore <- DBI::dbGetQuery(con, "SELECT count(*) FROM positions.postable")
      DBI::dbSendQuery(con, "SET search_path TO positions, public")


      for(i in 1:length(delFiles)){
        toDelete<-paste0("DELETE
                   FROM positions.postable
                   WHERE posdata_file = '",
                         delFiles[i],
                         "'")

        result <- dbSendQuery(con, toDelete)
      }


      nRowsAfter <- DBI::dbGetQuery(con, "SELECT count(*) FROM positions.postable")

      ##Get rowsToDelete
      nRowsDeleted <- nRowsBefore - nRowsAfter

      if(nRowsDeleted != nRowsToDelete){
        dbBreak()
        return("Not all lines could be deleted, aborted!")

      }

    })


      return(paste0("All ", nRowsToDelete, " lines deleted, attributed to ", sumFilesExisting, " posdata files."))


}


