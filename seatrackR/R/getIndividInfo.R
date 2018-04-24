#' Retrieve info on the individuals
#'
#' This is a convenience function that pulls together various info on the files in the individuals.infivid_info and individuals.individ_status table and other tables
#'
#' @return Data frame.
#' @export
#' @examples
#' dontrun{
#' seatrackConnect(Username = "testreader", Password = "testreader")
#' individInfo <- getInfividInfo()
#' }



getIndividInfo <- function(selectColony = NULL,
               selectYear = NULL){

  checkCon()


  sessions <- dplyr::tbl(con, dbplyr::in_schema("loggers", "logging_session"))

  individs <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_info"))

  status <- dplyr::tbl(con, dbplyr::in_schema("individuals", "individ_status"))

  deployments <- dplyr::tbl(con, dbplyr::in_schema("loggers", "deployment")) %>%
    select(session_id,
           status_date = deployment_date) %>%
    mutate(eventType = "Deployment")

  retrievals <- dplyr::tbl(con, dbplyr::in_schema("loggers", "retrieval")) %>%
    select(session_id,
           status_date = retrieval_date) %>%
    mutate(eventType = "Retrieval")

  events <- as_tibble(deployments) %>%
    bind_rows(as_tibble(retrievals))

  if(!is.null(selectColony)){
    sessions <- sessions %>% filter(colony %in% selectColony)
  }

  if(!is.null(selectYear)){
    sessions <- sessions %>% filter(year_tracked %in% selectYear)
  }

##Not done. get only 270 records now. Check join! want to make sure the status updates are linked to the correct sessions.
query <-  sessions %>%
  inner_join(status, by = c("session_id" = "session_id"))  %>%
  left_join(individs, by = c("individ_id" = "individ_id")) %>%
  select(session_id,
         colony,
         year_tracked,
         ring_number = ring_number.x,
         euring_code = euring_code.x,
         color_ring = color_ring.x,
         species = species.x,
         subspecies = subspecies.x,
         morph = morph.x,
         age = age.x,
         sex = sex.x,
         sexing_method = sexing_method.x,
         status_date,
         weight,
         scull,
         tarsus,
         wing,
         breeding_stage,
         eggs,
         chicks,
         hatching_success,
         breeding_success,
         breeding_success_criterion,
         data_responsible,
         back_on_nest,
         comment,
         status_sex = sex.y,
         status_sexing_method = sexing_method.y,
         status_age = age.y
         )

  out <- as_tibble(query) %>%
    left_join(events,
              by = c("session_id" = "session_id",
                     "status_date" = "status_date")) %>%
    arrange(colony,
            species,
            year_tracked,
            ring_number,
            status_date)


 return(out)

}