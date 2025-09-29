#' Retrieve summary info on the registered raw-files
#'
#' This is a convenience function that pulls together various info on the files in the loggers.file_archive table and other tables
#'
#' @return Data frame.
#' @export
#' @examples
#' dontrun{
#' seatrackConnect(Username = "testreader", Password = "testreader")
#' fileArchive <- getFileArchive()
#' }

getFileArchiveSummary <- function(colony = NULL,
                           year = NULL){
  seatrackR:::checkCon()

  res <- DBI::dbGetQuery(con,
  "SELECT f.file_id, f.session_id, ls.colony, ii.ring_number, ii.euring_code, ls.year_tracked, li.logger_serial_no, li.logger_model, f.filename
  FROM loggers.file_archive f, loggers.logging_session ls, individuals.individ_info ii, loggers.logger_info li
  WHERE f.session_id = ls.session_id
  AND ls.logger_id = li.logger_id
  AND ls.individ_id = ii.individ_id
  ORDER BY file_id")  %>% as_tibble()


if(!is.null(colony)){
  res <- res %>% filter(colony %in% colony)
}

if(!is.null(year)){
  res <- res %>% filter(year_tracked %in% year)
}

return(res)

}

##Not finished. Need to code the filetypes into fewer categories
getFileArchiveSummary2 <- function(colony = NULL,
                                   year = NULL){

  seatrackR:::checkCon()

  fileQ <- "SELECT li.logger_id, li.logger_serial_no, li.logger_model, ls.year_tracked, date_part('year', r.retrieval_date) as year_retrieved,
  s.logging_mode, ii.ring_number, ii.euring_code, ls.colony, d.deployment_date as date_deployed, r.retrieval_date date_retrieved,
  'TO BE ADDED'as logger_date_failed, 'TO BE ADDED'as data_in_trn,  ls.species,
  f.file_id, f.session_id, f.filename, f.file_basename
  FROM loggers.file_archive f,
  loggers.logging_session ls,
  individuals.individ_info ii,
  loggers.logger_info li,
  loggers.retrieval r,
  loggers.startup s,
  loggers.deployment d
  WHERE f.session_id = ls.session_id
  AND ls.session_id = r.session_id
  AND ls.session_id = s.session_id
  AND ls.session_id = d.session_id
  AND ls.logger_id = li.logger_id
  AND ls.individ_id = ii.individ_id"

  res <- dbGetQuery(con, fileQ)

  if(!is.null(colony)){
    res <- res %>% filter(colony %in% colony)
  }

  if(!is.null(year)){
    res <- res %>% filter(year_tracked %in% year)
  }


  out <- res %>%
    as_tibble %>%
    select(-file_id) %>%
    group_by(session_id) %>%
    spread(key = file_basename, val = filename, drop = T)  %>%
    arrange(session_id) %>%
    ungroup()


  return(out)
}
