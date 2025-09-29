#' Download files from the file archive
#'
#'
#'
#'
#' @return Status messages on the actions taken for each file.
#' @export
#' @examples
#' dontrun{
#' ##To download all files in the file archive.
#' myFiles = listFileArchive()$filesInArchive
#' downloadFiles(files = myFiles, destFolder = "temp")
#' }
#'


downloadFiles <- function(files = NULL,
                          destFolder = NULL,
                          overwrite = F){
  seatrackR:::checkCon()

  if(!tibble::is_tibble(files)) files <- tibble::tibble(filename = files)

  archive <- listFileArchive()

  notThere <- files$filename[!(files$filename %in% archive$filesInArchive$filename)]
  if(length(notThere) > 0) stop(c("Requested files are not in archive: \n", paste(notThere, "\n")))

  url <- seatrackR:::.getFtpUrl()

  tempEnv <- new.env(parent = as.environment("package:seatrackR"))

  getFile <- function(x, destFolder = destFolder, overwrite = overwrite){

    if(!is.null(destFolder)){
      filename <- paste0(destFolder, "/", x)

    } else {
      filename <- paste(x)
    }

    if(file.exists(filename) & overwrite == F){
      warning(paste("File", filename, "already exists, use overwrite = True to overwrite"))
      return(paste0("File not downloaded: ", filename))
    } else {
      tmp <- strsplit(url$url, "//")
      getUrl <- paste0(tmp[[1]][1], "//", url$pwd, "@", tmp[[1]][2],"/" , x)
      getHandle <- httr::handle(getUrl)



       mess  <- lapply(getUrl, seatrackR:::factory(function(x){
         rawOut <- httr::with_config(httr::config(ssl_verifypeer = F,
                                                   ssl_verifyhost = F,
                                                   use_ssl = T),
                                      httr::GET(url = x, handle = getHandle))
         assign("rawOut", rawOut, envir = tempEnv)
         }))


       if(!is.null(mess[[1]]$err)){
         return(paste(mess[[1]]$err, ":", x))
         } else {
           bin <- httr::content(tempEnv$rawOut, type = "raw")
           writeBin(bin, paste0(destFolder, "/", x))
         }

      ##Need to include a working positive status message

    }

  }

  apply(files, 1, function(x) getFile(x, overwrite = overwrite, destFolder = destFolder))
}



