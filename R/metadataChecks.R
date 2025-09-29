#' checkMetadata before import
#'
#' This is a collection of functions to check the integrity of the metadata table before importing it.
#'
#'
#' @param myTable
#'
#' @return Various errors.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' myCheck <- checkMetadata(sampleMetadata)
#' plot(myCheck) #quickly see how many problems there are
#' myCheck # check the print function for a complete list of the errors
#'
#' #Or run a single test separately
#' checkOpenSession(sampleMetadata)
#' }


checkMetadata <- function(myTable){
  checkCon()
  sessionErrors <- checkOpenSession(myTable)
  loggerErrors <- checkLoggers(myTable)
  nameErrors <- checkNames(myTable)
  retrievedIDError <- checkRetrievedMatchDeployed(myTable)

  allErrors <- list(sessionErrors,
                    loggerErrors,
                    nameErrors,
                    retrievedIDError)
  class(allErrors) <- c("metadataErrors", "list")


  if(length(unlist(allErrors)) == 0) {
    message("No errors found")
  } else {
    message("Errors found!")
  }

  return(allErrors)
}

#' @describeIn checkMetadata Check loggers to be deployed or retrieved is in an open logging session
#' @export
checkRetrievedMatchDeployed <- function(myTable){

  #This is not done. Need to merge deployments in myTable to those in database before performing check.
    checkCon()


  activeDataQ <- "SELECT logging_session.session_id,
logger_info.logger_model,
logger_info.logger_serial_no,
individ_info.ring_number,
individ_info.euring_code,
date_part('year', starttime_gmt) as \"startYear\"
   FROM loggers.logging_session LEFT JOIN loggers.logger_info ON logging_session.logger_id =	logger_info.logger_id
 LEFT JOIN individuals.individ_info ON logging_session.individ_id = individ_info.individ_id LEFT JOIN loggers.startup ON logging_session.session_id = startup.session_id
  WHERE logging_session.active IS True
   "

activeData <- DBI::dbGetQuery(con, activeDataQ)

deployedIndividualsQ <- "SELECT * FROM loggers.deployment"

deployedIndividuals <- DBI::dbGetQuery(con, deployedIndividualsQ)

myTableDeployedIndividuals <- myTable %>%
  transform(startYear = as.integer(as.character(format(date, "%Y"))))

myTableDeployedIndividuals <- activeData %>%
  left_join(myTableDeployedIndividuals,
             by = c("logger_model" = "logger_model_deployed",
                    "logger_serial_no" = "logger_id_deployed",
                    "startYear" = "startYear")) %>%
  select(session_id,
         ring_number.y,
         euring_code.y,
         startYear)

deployedIndividuals <- deployedIndividuals %>%
  right_join(myTableDeployedIndividuals,
             by = c("session_id" = "session_id"))

myTableRetrievedIndividuals <- myTable %>%
  transform(startYear = as.integer(as.character(format(date, "%Y"))) - 1) ##CHECK THIS


retrievedIndividuals <- activeData %>%
  inner_join(myTableRetrievedIndividuals,
             by = c("logger_model" = "logger_model_retrieved",
                    "logger_serial_no" = "logger_id_retrieved",
                    "startYear" = "startYear"))


retrievedNotDepl <- retrievedIndividuals %>%
  left_join(deployedIndividuals,
            by = c("session_id" = "session_id")) %>%
  filter(ring_number.y.x != ring_number.y.y) %>%
  select(individ_id, ring_number = ring_number.y.x,
         euring_code = euring_code.y.x,
         session_id,
         logger_model,
         logger_serial_no,
         logger_id)

out <- list()
out$retrievedRingNotDeployed <- retrievedNotDepl

return(out)

}


#' @describeIn checkMetadata Check loggers to be deployed or retrieved is in an open logging session
#' @export
checkOpenSession <- function(myTable){
 checkCon()

  activeSessions <- DBI::dbGetQuery(con,
             "SELECT li.logger_serial_no, li.logger_model
              FROM loggers.logging_session ls, loggers.logger_info li
              WHERE li.logger_id = ls.logger_id
              AND ls.active IS True")


  whichDepNotActive <- !(myTable$logger_id_deployed %in% activeSessions$logger_serial_no & myTable$logger_model_deployed %in% activeSessions$logger_model)
  whichRetrNotActive <- !(myTable$logger_id_retrieved %in% activeSessions$logger_serial_no & myTable$logger_model_retrieved %in% activeSessions$logger_model)

  deployedNotActive <- myTable$logger_id_deployed[whichDepNotActive]
  retrievedNotActive <- myTable$logger_id_retrieved[whichRetrNotActive]

  out <- list()

  out$deployedNotActive <- data.frame("row_number" = which(whichDepNotActive)[!is.na(deployedNotActive)], "logger_serial_no" = deployedNotActive[!is.na(deployedNotActive)])
  out$retrievedNotActive <- data.frame("row_number" = which(whichRetrNotActive)[!is.na(retrievedNotActive)], "logger_serial_no" = retrievedNotActive[!is.na(retrievedNotActive)])
  return(out)
  }

#' @describeIn checkMetadata Check if loggers are registered in the loggers.logger_info table
#' @export
checkLoggers <- function(myTable){
  checkCon()

  registeredLoggers <- DBI::dbGetQuery(con,
                                    "SELECT * FROM loggers.logger_info")


  whichDepNotReg <- !(myTable$logger_id_deployed %in% registeredLoggers$logger_serial_no & myTable$logger_model_deployed %in% registeredLoggers$logger_model)
  whichRetrNotReg <- !(myTable$logger_id_retrieved %in% registeredLoggers$logger_serial_no & myTable$logger_model_retrieved %in% registeredLoggers$logger_model)

  deployedNotReg <- myTable$logger_id_deployed[whichDepNotReg]
  retrievedNotReg <- myTable$logger_id_retrieved[whichRetrNotReg]

  out <- list()

  out$deployedNotReg <- data.frame("row_number" = which(whichDepNotReg)[!is.na(deployedNotReg)], "logger_serial_no" = deployedNotReg[!is.na(deployedNotReg)])
  out$retrievedNotReg <- data.frame("row_number" = which(whichRetrNotReg)[!is.na(retrievedNotReg)], "logger_serial_no" = retrievedNotReg[!is.na(retrievedNotReg)])
  return(out)
}


#' @describeIn checkMetadata Check if names in data_responsible exists in the metadata.people table.
#' @export
#'
checkNames <- function(myTable){
  checkCon()

  presentNames <- DBI::dbGetQuery(con,
                                    "SELECT * FROM metadata.people")

  whichPeopleNotPresent <- !(myTable$data_responsible %in% presentNames$name)

  peopleNotPresent <- myTable$data_responsible[whichPeopleNotPresent]

  out <- list()
  out$peopleNotPresent <- data.frame("row_number" = which(whichPeopleNotPresent), "name" = peopleNotPresent)

  return(out)
}

#' @export
print.metadataErrors <- function(x){

  if (length(unlist(x)) == 0){
    cat("No errors found (through the available checking functions)")
  } else

  if(nrow(x[[1]][[1]]) > 0){
    cat("These loggers are not in an open logging session, but metadata contains deployment info.\nStart a new logging session with writeLoggerImport() before importing deployment info.\n")
    print(x[[1]][[1]])
  }

  if(nrow(x[[1]][[2]]) > 0){
    cat("\n")
    cat("These loggers are not in an open logging session, but metadata contains retrieval info.\nStart a new logging session with writeLoggerImport(), and add deployment info before importing retrieval info.\n")
    print(x[[1]][[2]])
  }

  if(nrow(x[[2]][[1]]) > 0){
    cat("\n")
    cat("These loggers are not registered in the table loggers.logger_info, but metadata contains deployment info.\nRegister the loggers with writeLoggerImport() before importing deployment info.\n")
    print(x[[2]][[1]])
  }

  if(nrow(x[[2]][[2]]) > 0){
    cat("\n")
    cat("These loggers are not registered in the table loggers.logger_info, but metadata contains retrieval info.\nRegister the loggers with writeLoggerImport(), and add deployment info before importing retrieval info.\n")
    print(x[[2]][[2]])
  }

  if(nrow(x[[4]][[1]]) > 0){
    cat("\n")
    cat("These retrieved ring numbers don't match the ring numbers that where deployed on this logger. \nIndivid_id of NA means the deployment data is not yet in database. \n")
    print(x[[4]][[1]])

  }

  if(nrow(x[[3]][[1]]) > 0){
    cat("\n")
    cat("These names are not in the table metadata.people. Check spelling and compare with getNames().\n")
    print(x[[3]][[1]])

  }

}

#' @export
plot.metadataErrors <- function(x, ...){
  if(length(unlist(x)) == 0) {
    plot(1:10, type = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
    text(6, 5, labels = "No errors found \n(through the available checking functions)")
  } else{
    toPlot <- tibble("Error_type" = unlist(lapply(x, names)),
                     "errorCount" = c(unlist(lapply(x[[1]], nrow)),
                                      unlist(lapply(x[[2]], nrow)),
                                      unlist(lapply(x[[3]], nrow)),
                                      unlist(lapply(x[[4]], nrow))))

    ggplot2::ggplot(toPlot) +
      ggplot2::geom_bar(mapping = aes(x = Error_type, y = errorCount, fill = Error_type), stat = "identity") +
      ggplot2::theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      ggplot2::ggtitle("Number of errors in metadata") +
      scale_fill_discrete(name = "Error type")

  }

}


#' @export
summary.metadataErrors <- function(x, ...){

    out <- tibble("reason" = unlist(lapply(x, names)),
                     "errorCount" = c(unlist(lapply(x[[1]], nrow)),
                                      unlist(lapply(x[[2]], nrow)),
                                      unlist(lapply(x[[3]], nrow)),
                                      unlist(lapply(x[[4]], nrow))))

  return(out)

}


