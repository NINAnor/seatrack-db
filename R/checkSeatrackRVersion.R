#' checkSeatrackRVersion
#'
#' Search the github repository to compare the current installed version of the package
#'
#'
#'
#'
#'
#' @export


checkSeatrackRVersion <- function(){

  pkg = "seatrackR"

  installed_version <- tryCatch(packageVersion(gsub(".*/",
                                                    "", pkg)), error = function(e) NA)

  url <- "https://raw.githubusercontent.com/NINAnor/seatrack-db/master/seatrackR/DESCRIPTION"

  x <- readLines(url)
  #remote_version <- gsub("(.*)(Version: )(.*)(<.*)", "\\3", grep("Version:", x, value = T))
  remote_version <- gsub("(Version: )(.*)", "\\2", grep("Version:", x, value = T))

  res <- list(package = pkg, installed_version = installed_version,
              latest_version = remote_version, up_to_date = NA)

  if (is.na(installed_version)) {
    message(paste("##", pkg, "is not installed..."))
  } else {
    if (remote_version > installed_version) {
      msg <- paste("##", pkg, "is out of date, latest version is",
                   remote_version)
      message(msg)
      res$up_to_date <- FALSE
    }
    else if (remote_version == installed_version) {
      message("seatrackR is up-to-date")
      res$up_to_date <- TRUE
    }
  }
  return(res)
}
