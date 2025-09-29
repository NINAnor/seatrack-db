#' Read logger recordings data
#'
#' This is a convenience function that reads from the "activity" tables temperature, activity, and light in the schema Recordings
#'
#' @param type light, temperature, or activity as a character. Default = "light".
#' @param session_id subset data for a character vector of session ids
#' @param individ_id subset data for a character vector of individual ids
#' @param colony subset data for a character vector of colony names (International names)
#' @param species subset data for a character vector of species
#' @param year_tracked subset data for a character vector of year_tracked (e.g. 2014_15)
#' @param asTibble Boolean. Return result as Tibble instead of lazy query? Tibble is slower, but also here forces the timezone to "UTC".
#'
#' @return A Lazy query or optionally a Tibble.
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#' getRecordings(type = "temperature",
#'              colony = "Sklinna")
#' }


getRecordings <- function(type = NULL,
                          sessionId = NULL,
                          individId = NULL,
                          colony = NULL,
                          species = NULL,
                          yearTracked = NULL,
                          asTibble = T){
  seatrackR:::checkCon()

  type <- match.arg(type, choices = c("light", "temperature", "activity"))

  sourceTbl <- dplyr::tbl(con, dbplyr::in_schema("recordings", type))
  sessionTbl <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))


  temp <- sourceTbl %>%
    inner_join(sessionTbl, by = c("session_id" = "session_id"))

  if(!is.null(sessionId)){
    sessionFilter <- as.character(sessionId)
    temp <- temp %>%
      filter(session_id %in% sessionFilter)
  }

  if(!is.null(individId)){
    individFilter <- as.character(individId)
    temp <- temp %>%
      filter(individ_id.y %in% individFilter)
  }

  if(!is.null(colony)){
    colonyFilter <- as.character(colony)
    temp <- temp %>%
      filter(colony %in% colonyFilter)
  }

  if(!is.null(species)){
    speciesFilter <- as.character(species)
    temp <- temp %>%
      filter(species %in% speciesFilter)
  }

  if(!is.null(yearTracked)){
    yearFilter <- as.character(yearTracked)
    temp <- temp %>%
      filter(year_tracked %in% yearFilter)
  }

  if(type == "light"){
    temp <- temp %>%
      select(session_id = session_id,
             individ_id = individ_id.x,
             filename,
             date_time,
             clipped,
             raw_light,
             std_light
             )
  }

  if(type == "temperature"){
    temp <- temp %>%
      select(session_id = session_id,
             individ_id = individ_id.x,
             filename,
             date_time,
             wet_temp_min,
             wet_temp_max,
             wet_temp_mean,
             num_samples
      )

  }

  if(type == "activity"){
    temp <- temp %>%
      select(session_id = session_id,
             individ_id = individ_id.x,
             filename,
             date_time,
             conductivity,
             std_conductivity
      )

  }

  res <- temp

  if(asTibble){
    res <- res %>% dplyr::collect()

    #Force timezone on date_time to UTC
    res <- res %>%
      mutate(date_time = lubridate::force_tz(date_time,
                                             tzone = "UTC"))
  }




  return(res)
}


