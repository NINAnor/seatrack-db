#' Retrieve info on the registered species in the database
#'
#'
#'
#' @return A tibble of the metadata.subspecies table
#' @export
#' @examples
#' \dontrun{
#' getSpecies()
#' }


getSpecies <- function(){
  checkCon()

  species <- dbReadTable(con, DBI::Id(schema = "metadata", table = "subspecies"))

  out <- as_tibble(species) %>%
    select(species_name_eng,
           species_name_latin,
           sub_species)

  return(out)
}







