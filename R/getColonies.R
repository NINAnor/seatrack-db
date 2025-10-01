#' Retrieve info on the registered colonies and locations within colonies in the database
#'
#' This function either reads from the metadata.colony or the metadata.location table, depending on the parameter allLocations.
#' If
#'
#' @param allLocations True, False. Should all locations within colonies be loaded. Default = False.
#' @param loadGeometries True, False. Should the geometries be loaded as an sf object. Default = False.
#'
#' @return A tibble of the metadata.colony or metadata.location table with or without sf geometry.
#' @export
#' @examples
#' \dontrun{
#' colony <- getColonies(loadGeometries = T)
#' plot(colony["colony_int_name"],
#' pch = 16)
#' }


getColonies <- function(allLocations = F,
                        loadGeometries = F){
  checkCon()

  if(allLocations == T &
   loadGeometries == F){

  locations <- dbReadTable(con, DBI::Id(schema = "metadata", table = "location"))
  out <- as_tibble(locations) %>%
    select(location_name,
           colony_int_name,
           colony_nat_name)
  return(out)
  }

  if(allLocations == F &
     loadGeometries == F){

    locations <- dbReadTable(con, DBI::Id(schema = "metadata", table = "colony"))
    out <- as_tibble(locations) %>%
      select(colony_int_name,
             colony_nat_name)
    return(out)
  }


  if(allLocations == T &
     loadGeometries == T){

    locations <- sf::st_read(con, DBI::Id(schema = "metadata", table = "location"))
    out <- as_tibble(locations) %>%
      sf::st_as_sf() %>%
      select(location_name,
             colony_int_name,
             colony_nat_name)
    return(out)
  }

  if(allLocations == F &
     loadGeometries == T){

    locations <- sf::st_read(con, DBI::Id(schema = "metadata", table = "colony"))
    out <- as_tibble(locations) %>%
      sf::st_as_sf() %>%
      select(colony_int_name,
             colony_nat_name)
    return(out)
  }

}







