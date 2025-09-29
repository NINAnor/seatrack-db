#' View the postable table
#'
#' This is a convenience function that reads from the table "positions.postable".
#' This table contains the primary positions data.
#'
#' @param species Character string. Option to limit selection to one or a set of species.Default is NULL, indicating all species.
#' The available choices can be seen in the column `species_name_eng` in the result from the function `getSpecies()`.
#' @param colony Character string. Option to limit selection to one or a set of colonies. Default is NULL. The available
#' choices can be seen in the column `colony_int_name` in the result from the function `getColonies()`.
#' @param dataResponsible Character string. Option to limit selection to one or a set of names of data responsible persons. Note that this
#' must conform to the name nomenclature used in the postable. Default is NULL. The available
#' choices can be seen in the column `name` in the result from the function `getNames()`.
#' @param ringnumber Character string. Option to limit selection to one or a set of ring numbers. Default is NULL.
#' @param year Character string. Option to limit selection to one or more years that the logging sessions span. The availablle
#' choices can be found in the `year_tracked` column in the result form the `getYears` function.
#' @param loadGeometries True, False. Should geometries be loaded from the columns lon_smooth2 and lat_smooth2?
#' If True, the returned object is a simple features object with only rows of eqfilter3 == 1, and with lat_smooth2 and lon_smooth2 values. Default = False
#' @param limit FALSE or Integer. Limit the number of rows returned to this number. Default = False.
#' @param loadImportDate Boolean. Should results include the import date to the database? Default = True.
#' @param asTibble Boolean. Should the result be given as a tibble instead of a lazy query? Tibble is slower, but also here forces the timezone to "UTC".
#' @return A lazy query or optionally a tibble of postable records. In the case of loadGeometries = T (default), also of class sf (simple feature) with geometries based on lat lon.
#' @import dplyr
#' @export
#' @examples
#' dontrun{
#' connectSeatrack(Username = "testreader", Password = "testreader")
#'
#' positions <- getPosdata(colony = "Kongsfjorden",
#'                 dataResponsible = "Sebastien Descamps",
#'                 species = "Little auk",
#'                 limit = F,
#'                 loadGeometries = F)
#'
#' positions
#'
#' # get data with geometries (default)
#' positions2 <- getPosdata(limit = 500)
#'
#' positions2
#'
#' #make a simple plot
#' plot(positions2["logger_model"], pch = 16)
#'
#' }
#' @export
getPosdata <- function(species= NULL,
                       colony = NULL,
                       dataResponsible = NULL,
                       ringnumber = NULL,
                       year = NULL,
                       sessionId = NULL,
                       individId = NULL,
                       loadGeometries = F,
                       loadImportDate = T,
                       asTibble = T,
                       limit = F){

  seatrackR:::checkCon()

  selectSpecies <- species
  selectColony <- colony


  if(!limit == F & !is.numeric(limit)){
    stop("limit must be FALSE or a numeric value")
  }

  postable <- tbl(con, dbplyr::in_schema("views", "postable"))
  res <- postable

  if(!is.null(selectSpecies)){
    res <- res %>% filter(species %in% selectSpecies)
  }

  if(!is.null(selectColony)){
    res <- res %>% filter(colony %in% selectColony)
  }

  if(!is.null(dataResponsible)){
    res <- res %>% filter(data_responsible %in% dataResponsible)
  }

  if(!is.null(ringnumber)){
    res <- res %>% filter(ring_number %in% ringnumber)
  }

  if(!is.null(year)){
    res <- res %>% filter(year_tracked %in% year)
  }

  if(!is.null(sessionId)){
    res <- res %>% filter(session_id %in% sessionId)
  }

  if(!is.null(individId)){
    res <- res %>% filter(individ_id %in% individId)
  }


  if(!loadImportDate){
    res <- res %>% select(-import_date)
  }

  if(!limit == F){
    res <- res %>% head(limit)
  }

  if(asTibble){
    res <- res %>% dplyr::collect()

    #Force timezone on date_time to UTC
    res <- res %>%
      mutate(date_time = lubridate::force_tz(date_time,
                                             tzone = "UTC"))
  }

  if(loadGeometries == T){
    res <- res %>% filter(!is.na(lon_smooth2) &
                            !is.na(lat_smooth2) &
                            eqfilter3 == 1)

    res <- res %>%
     dplyr::collect() %>%
      sf::st_as_sf(coords = c("lon_smooth2", "lat_smooth2"),
                                crs = 4326,
                                remove = F)
  }



  return(res)

}

