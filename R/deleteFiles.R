#' deleteFiles
#'
#'
#'
#' @return Status messages on the actions taken for each file.
#' @export
#' @examples
#' dontrun{
#'
#' deleteFiles(files = c("test.txt", "test_file2.txt"), originFolder = "temp")
#' }
#'
#'

##Use httr::with_options(...,
#DELETE())

deleteFiles <- function(files = NULL, force = F, ...){

  checkCon()

  current_user <- DBI::dbGetQuery(con, "SELECT current_user")

  current_roles <- DBI::dbGetQuery(con, paste0("select rolname from pg_user
                                             join pg_auth_members on (pg_user.usesysid=pg_auth_members.member)
                                             join pg_roles on (pg_roles.oid=pg_auth_members.roleid)
                                             where
                                             pg_user.usename = '", current_user, "'"))
  current_roles <- current_roles[,1]


  if(!("admin" %in% current_roles || "seatrack_writer" %in% current_roles)) stop("Connected user needs to be part of seatrack_writer or admin group")


  if(!tibble::is_tibble(files)) files <- tibble::as_tibble(x = files)

  names(files) <- "filename"

  fileArchive <- listFileArchive()

  url <- seatrackR:::.getFtpUrl()


  deleteFile <- function(x, url, ...){

    tmp <- strsplit(url$url, "//")
    dest <- paste0(tmp[[1]][1], "//", url$pwd, "@", tmp[[1]][2], "/", x)

    getHandle <- httr::handle(dest)

    mess  <- lapply(x, factory(function(x){

      httr::with_config(httr::config(ssl_verifypeer = F,
                                   ssl_verifyhost = F,
                                   use_ssl = T,
                                   dirlistonly = T,
                                   customrequest = paste0("DELE ", x),
                                   filetime = F,
                                   ...),
                      httr::POST(url = dest, handle = getHandle))



      }))
  }


  if(any(!(files$filename %in% fileArchive$filesInArchive$filename))){

    notThere <- files %>%
      filter(!(filename %in% fileArchive$filesInArchive$filename))

    files <- paste0(notThere$filename, "\n")
    stop(gettext("These files marked for deletion are not present in the file archive:\n", files))
  } else {

    if(force == F){
      there <- files %>%
        filter((filename %in% fileArchive$filesInArchive$filename))

      there <- paste0(there$filename, collapse = "\n")

      answer <- menu(c("Yes (1)", "No (2)"), title = paste0("You are about to delete these files: \n", there, collapse = ":"), " records. Are you sure?")

      if(answer == 2){
        return("No files deleted")
      }

    }


     apply(files, 1, function(x) deleteFile(x = x, url = url))


    newStatus <- listFileArchive()
    deletedFiles <- fileArchive$filesInArchive$filename[!(fileArchive$filesInArchive$filename %in% newStatus$filesInArchive$filename)]


    if(length(deletedFiles) == 0){
      return("No files deleted")} else {
        return(paste0("File deleted: ", deletedFiles))
    }

  }
}




