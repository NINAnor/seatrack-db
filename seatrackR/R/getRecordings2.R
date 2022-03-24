#' Read logger recordings data
#'
#' This is a convenience function that reads from the "activity" tables temperature, activity, and light in the schema Recordings.
#' This version uses the terminal command psql to bypass R and its packages DBI and PostgreSQL. This is an attempt to allow for larger downloads
#' where the db-connection in R breaks down.
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


getRecordings2 <- function(type = NULL,
                          sessionId = NULL,
                          individId = NULL,
                          colony = NULL,
                          species = NULL,
                          yearTracked = NULL,
                          asTibble = T){
  #seatrackR:::checkCon()

  try(host <- system("hostname", intern = T))
  if(!grepl("ninrstudio", host)) stop("Must be run on one of NINAs Linux servers!")

  type <- match.arg(type, choices = c("light", "temperature", "activity"))

  sourceTbl <- paste0("recordings.", type, " as rec")

  sessionTbl <- "loggers.logging_session as ls"

  if(type == "light"){

    stub_q <- paste0("SELECT rec.session_id,
             rec.individ_id,
             rec.filename,
             rec.date_time,
             rec.clipped,
             rec.raw_light,
             rec.std_light
             FROM ",
    sourceTbl,
    ",\n",
    sessionTbl,
    "\nWHERE rec.session_id = ls.session_id"


              )

    }


  if(type == "temperature"){

    stub_q <- paste0("SELECT rec.session_id,
             rec.individ_id,
             rec.filename,
             rec.date_time,
             rec.wet_min,
             rec.wet_max,
             rec.wet_mean,
             rec.num_samples
             FROM ",
                     sourceTbl,
                     ",\n",
                     sessionTbl,
                     "\nWHERE rec.session_id = ls.session_id"

    )
  }


  if(type == "activity"){

    stub_q <- paste0("SELECT rec.session_id,
             rec.individ_id,
             rec.filename,
             rec.date_time,
             rec.conductivity,
             rec.std_conductivity
             FROM ",
             sourceTbl,
             ",\n",
             sessionTbl,
             "\nWHERE rec.session_id = ls.session_id"

    )
  }



  if(!is.null(sessionId)){
    sessionFilter <- paste0("'", sessionId, sep = "',", collapse = "")
    sessionFilter <- stringr::str_sub(sessionFilter,1,nchar(sessionFilter)-1)
    sessionFilter <- paste0("(", sessionFilter, ")")

    stub_q <- paste0(stub_q,
                     "\nAND rec.session_id IN ",
                     sessionFilter)

  }


  if(!is.null(individId)){
    individFilter <- paste0("'", individId, sep = "',", collapse = "")
    individFilter <- stringr::str_sub(individFilter,1,nchar(individFilter)-1)
    individFilter <- paste0("(", rec.individFilter, ")")

    stub_q <- paste0(stub_q,
                     "\nAND rec.individ_id IN ",
                     individFilter)
  }

  if(!is.null(colony)){
    colonyFilter <- paste0("'", colony, sep = "',", collapse = "")
    colonyFilter <- stringr::str_sub(colonyFilter,1,nchar(colonyFilter)-1)
    colonyFilter <- paste0("(", colonyFilter, ")")

    stub_q <- paste0(stub_q,
                     "\nAND ls.colony IN ",
                     colonyFilter)
  }

  if(!is.null(species)){
    speciesFilter <- paste0("'", species, sep = "',", collapse = "")
    speciesFilter <- stringr::str_sub(speciesFilter,1,nchar(speciesFilter)-1)
    speciesFilter <- paste0("(", speciesFilter, ")")

    stub_q <- paste0(stub_q,
                     "\nAND ls.species IN ",
                     speciesFilter)
  }

  if(!is.null(yearTracked)){
    yearTrackedFilter <- paste0("'", yearTracked, sep = "',", collapse = "")
    yearTrackedFilter <- stringr::str_sub(yearTrackedFilter,1,nchar(yearTrackedFilter)-1)
    yearTrackedFilter <- paste0("(", yearTrackedFilter, ")")

    stub_q <- paste0(stub_q,
                     "\nAND year_tracked IN ",
                     yearTrackedFilter)
  }

  query <- paste( "\\copy (", stub_q, ") To '/data/scratch/temp_seatrack.csv' With CSV DELIMITER ',' HEADER")

  pass <- get(".pass", envir = as.environment(passEnv))
  user <- get(".user", envir = as.environment(passEnv))

  system_call <- paste0("export PGPASSWORD='", pass,"';\n", "psql -h seatrack.nina.no -d seatrack -U ", user, " -c \"", query, "\" 2>&1")

  #Run the query
  cat("Running the query...")
  system(system_call)


  res <- readr::read_csv("/data/scratch/temp_seatrack.csv",
                         show_col_types = FALSE)

  # res <- temp
  #
  # if(asTibble){
  #   res <- res %>% dplyr::collect()
  #
  #   #Force timezone on date_time to UTC
  #   res <- res %>%
  #     mutate(date_time = lubridate::force_tz(date_time,
  #                                            tzone = "UTC"))
  # }
  #

  system("rm /data/scratch/temp_seatrack.csv")

  return(res)
}


