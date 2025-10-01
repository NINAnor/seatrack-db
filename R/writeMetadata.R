#' Import metadata
#'
#' This is a convenience function that writes to the "imports.metadata_import"
#'
#' The import_metadata table is one of the two major ways of importing data into the database.
#' Together with the table logger_import, this table handles all the routine information about loggers
#' and the fieldwork.
#'
#' @param metadaata A named vector or data frame that fits the metadata_import table in schema imports
#'
#' @return Data frame.
#' @export
#' @examples
#' \dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' writeMetadata(sampleMetadata)
#' }


writeMetadata <- function(metadata){
 checkCon()

  DBI::dbSendQuery(con, "SET search_path TO imports, public")
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbWriteTable(con, "metadata_import", metadata, append = T, overwrite = F)
    }
  )
}


##Not done, on to something...
writeMetadata2 <- function(metadata){
 checkCon()

  DBI::dbSendQuery(con, "SET search_path TO imports, public")


    testWrite <- function(myTable) {
      out <- tryCatch(
        {
          # Just to highlight: if you want to use more than one
          # R expression in the "try" part then you'll have to
          # use curly brackets.
          # 'tryCatch()' will return the last evaluated expression
          # in case the "try" part was completed successfully

          message("This is the 'try' part")

          DBI::dbWriteTable(con, "metadata_import", myTable, append = T, overwrite = F)
          # The return value of `readLines()` is the actual value
          # that will be returned in case there is no condition
          # (e.g. warning or error).
          # You don't need to state the return value via `return()` as code
          # in the "try" part is not wrapped insided a function (unlike that
          # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
          #message(paste("There was an error here", myTable))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(cond)
        },
        warning=function(cond) {
          #message(paste("This caused a warning", myTable))
          message("Here's the original warning message:")
          message(cond)
          # Choose a return value in case of warning
          return(NULL)
        },
        finally={
          # NOTE:
          # Here goes everything that should be executed at the end,
          # regardless of success or error.
          # If you want more than one expression to be executed, then you
          # need to wrap them in curly brackets ({...}); otherwise you could
          # just have written 'finally=<expression>'
          #message(paste("tested line", myTable))

        }
      )
      return(out)
    }

    testList <- split(sampleMetadata, seq(nrow(sampleMetadata)))
    testList <- list()
    testList[[1]] <- sampleMetadata[1, ]
    testList[[2]] <- sampleMetadata[2, ]

    y <- lapply(testList, testWrite)

  }





