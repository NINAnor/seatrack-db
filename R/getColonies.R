#' Retrieve info on the registered colonies and locations within colonies in the database
#'
#' This function either reads from the metadata.colony or the metadata.location table, depending on the parameter allLocations.
#' If
#'
#' @param allLocations True, False. Should all locations within colonies be loaded. Default = False.
#' @param loadGeometries True, False. Should the geometries be loaded as an sf object. Default = False.
#'
#' @return Either a tibble or a sf dataframe of the metadata.colony or metadata.location table.
#' @export
#' @examples
#' \dontrun{
#' colony <- getColonies(loadGeometries = T)
#' plot(colony["colony_int_name"],
#' pch = 16)
#' }


getColonies <- function(allLocations = FALSE, loadGeometries = FALSE){
  checkCon()

  if (allLocations) {
    locations <- dbReadTable(the$con, DBI::Id(schema = "metadata", table = "location"))
  } else {
    locations <- dbReadTable(the$con, DBI::Id(schema = "metadata", table = "colony"))
  }

  if (loadGeometries) {
    locations <- sf::st_as_sf(locations, na.fail = FALSE, remove = FALSE)
  }

  return(locations)

}