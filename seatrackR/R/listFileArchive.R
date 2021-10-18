#' Retrieve info on the status of files in the file archive
#'
#' This function checks which files are stored in the file archive, which files that are missing from the archive
#' (listed in the database but not present in the file archive), and which files are in the archive but not listed in the database (should be zero).
#'
#'
#'
#' @return A list.
#' @export
#' @examples
#' dontrun{
#' listFileArchive()
#' }
#' @seealso \code{\link{getFileArchive}} for a function that summarizes info on the files that should be in the file archive (connected to loggers that have been shut down). )


listFileArchive <- function(){
  checkCon()

  ##Get files in archive, using curl instead of RCurl
  url <- seatrackR:::.getFtpUrl()
  tmp <- strsplit(url$url, "//")
  dest <- paste0(tmp[[1]][1], "//", url$pwd, "@", tmp[[1]][2])

  list_files <- curl::new_handle()

  curl::handle_setopt(list_files,
                      ftp_use_epsv = TRUE,
                      dirlistonly = TRUE,
                      use_ssl = T,
                      ssl_verifyhost = F,
                      ssl_verifypeer = F,
                      sslversion = 6L)

  con <- curl::curl(url = dest, "r", handle = list_files)

  filesInStorage <- readLines(con)
  close(con)

  filesInStorage <- as_tibble(filesInStorage)
  names(filesInStorage) <- "filename"


  filesInDatabase <- getFileArchiveSummary()

  filesNotInStorage <- filesInDatabase %>%
    filter(!(filename %in% filesInStorage$filename)) %>%
    select(filename)

  filesNotInDatabase <- filesInStorage %>%
    filter(!(filename %in% filesInDatabase$filename))

  out <- list("filesInArchive" = filesInStorage, "filesNotInArchive" = filesNotInStorage, "filesNotInDatabase" = filesNotInDatabase)

  return(out)
}






