#' Delete activity data from the database, based on subselection criteria
#'
#' This is a convenience function that deletes records from the activity tables (records.activity, records.light, records.temp)
#'
#' @param selectSpecies Character string. Option to limit selection to one or a set of species.Default is NULL, indicating all species.
#' @param selectColony Character string. Option to limit selection to one or a set of colonies. Default is NULL.
#' @param selectUpdateTime Timestamp or character string that can be interpreted as a timestamp through as.POSIXct. Delete only
#' records that where last updated after this timestamp
#' @param selectSpecies Character string. Option to limit selection to one or a set of species. Default is NULL.
#' @param Force True, False (default = False). Skip confirmation check (for non interactive functionality)
#'
#' @return Status message
#' @import dplyr
#' @export
#' @examples
#' dontrun{
#' deleteActivity(selectUpdateTime = "2018-04-20")
#' }


deleteActivity <- function(colony = NULL,
                          intendedLocation = NULL,
                          year = NULL,
                          species = NULL,
                          updatedAfter = NULL,
                          updatedBefore = NULL,
                          updatedBy = NULL,
                          sessionId = NULL,
                          force = FALSE){

  seatrackR:::checkCon()

#append dummy condition to ease later conditions
deleteTemp<- "DELETE FROM recordings.temperature
                USING recordings.temperature as t
                LEFT OUTER JOIN loggers.logging_session as ls ON
                t.session_id = ls.session_id
                LEFT OUTER JOIN loggers.allocation a ON
                ls.session_id = a.session_id
                WHERE temperature.id = t.id"

#append dummy condition to ease later conditions
deleteAct<- "DELETE FROM recordings.activity
                USING recordings.activity as act
                LEFT OUTER JOIN loggers.logging_session as ls ON
                act.session_id = ls.session_id
                LEFT OUTER JOIN loggers.allocation a ON
                ls.session_id = a.session_id
                WHERE activity.id = act.id"

deleteLight<- "DELETE FROM recordings.light
                USING recordings.light as lig
                LEFT OUTER JOIN loggers.logging_session as ls ON
                lig.session_id = ls.session_id
                LEFT OUTER JOIN loggers.allocation a ON
                ls.session_id = a.session_id
                WHERE light.id = lig.id"



selectQueryTemp <- "SELECT count(distinct(filename)) FROM recordings.temperature as t
LEFT OUTER JOIN loggers.logging_session as ls ON
t.session_id = ls.session_id
LEFT OUTER JOIN loggers.allocation a ON
ls.session_id = a.session_id
WHERE 1=1"

selectQueryAct <- "SELECT count(distinct(filename)) FROM recordings.activity as act
                LEFT OUTER JOIN loggers.logging_session as ls ON
                act.session_id = ls.session_id
                LEFT OUTER JOIN loggers.allocation a ON
                ls.session_id = a.session_id
                WHERE 1=1"

selectQueryLight <- "SELECT count(distinct(filename)) FROM recordings.light as lig
                LEFT OUTER JOIN loggers.logging_session as ls ON
                lig.session_id = ls.session_id
                LEFT OUTER JOIN loggers.allocation a ON
                ls.session_id = a.session_id
                WHERE 1=1"


if(!is.null(colony)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.colony = '", colony, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.colony = '", colony, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.colony = '", colony, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND ls.colony = '", colony, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND ls.colony = '", colony, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND ls.colony = '", colony, "'")
}

if(!is.null(intendedLocation)){
  deleteTemp <- paste0(deleteTemp, "\nAND a.intended_location = '", intendedLocation, "'")
  deleteAct <- paste0(deleteAct, "\nAND a.intended_location = '", intendedLocation, "'")
  deleteLight <- paste0(deleteLight, "\nAND a.intended_location = '", intendedLocation, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND a.intended_location = '", intendedLocation, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND a.intended_location = '", intendedLocation, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND a.intended_location = '", intendedLocation, "'")
}


if(!is.null(year)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.year_tracked = '", year, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.year_tracked = '", year, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.year_tracked = '", year, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND a.intended_location = '", intendedLocation, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND a.intended_location = '", intendedLocation, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND a.intended_location = '", intendedLocation, "'")
}


if(!is.null(species)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.species = '", species, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.species = '", species, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.species = '", species, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND ls.species = '", species, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND ls.species = '", species, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND ls.species = '", species, "'")
}

if(!is.null(updatedAfter)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.last_updated > '", updatedAfter, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.last_updated > '", updatedAfter, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.last_updated > '", updatedAfter, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND ls.last_updated > '", updatedAfter, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND ls.last_updated > '", updatedAfter, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND ls.last_updated > '", updatedAfter, "'")
}

if(!is.null(updatedBefore)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.last_updated < '", updatedBefore, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.last_updated < '", updatedBefore, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.last_updated < '", updatedBefore, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND ls.last_updated < '", updatedBefore, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND ls.last_updated < '", updatedBefore, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND ls.last_updated < '", updatedBefore, "'")
}

if(!is.null(updatedBy)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.updated_by = '", updatedBy, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.updated_by = '", updatedBy, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.updated_by = '", updatedBy, "'")
  selectQueryTemp <- paste0(selectQueryTemp, "\nAND updated_by = '", updatedBy, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND updated_by = '", updatedBy, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND updated_by = '", updatedBy, "'")
}

if(!is.null(sessionId)){
  deleteTemp <- paste0(deleteTemp, "\nAND ls.session_id = '", sessionId, "'")
  deleteAct <- paste0(deleteAct, "\nAND ls.session_id = '", sessionId, "'")
  deleteLight <- paste0(deleteLight, "\nAND ls.session_id = '", sessionId, "'")

  selectQueryTemp <- paste0(selectQueryTemp, "\nAND ls.session_id = '", sessionId, "'")
  selectQueryAct <- paste0(selectQueryAct, "\nAND ls.session_id = '", sessionId, "'")
  selectQueryLight <- paste0(selectQueryLight, "\nAND ls.session_id = '", sessionId, "'")
}



noAffectedRowsTemp <- DBI::dbGetQuery(con, selectQueryTemp)
noAffectedRowsAct <- DBI::dbGetQuery(con, selectQueryAct)
noAffectedRowsLight <- DBI::dbGetQuery(con, selectQueryLight)

if(isTRUE(force)){
    DBI::dbExecute(con, deleteLight)
    DBI::dbExecute(con, deleteTemp)
    DBI::dbExecute(con, deleteAct)

} else {

  answer <- menu(c("Yes (1)", "No (2)"), title = paste0("Your selection corresponds to  \n",
                                                        noAffectedRowsTemp[1,1], " temperature files, \n",
                                                        noAffectedRowsAct[1,1], " activity files, \n",
                                                        noAffectedRowsLight[1,1], " light files, \n",
                                                        "Are you sure?"))

  if(answer == 1){

      DBI::dbExecute(con, deleteLight)
      DBI::dbExecute(con, deleteTemp)
      DBI::dbExecute(con, deleteAct)

  }


}

}

