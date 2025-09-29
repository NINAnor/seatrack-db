#' Write files to the file archive
#'
#'
#'
#' @return Status messages on the actions taken for each file.
#' @export
#' @examples
#' dontrun{
#'
#' uploadFiles(files = c("test_file.txt", "test_file2.txt"), originFolder = "temp")
#' }
#'


uploadFiles <- function(files = NULL,
                        originFolder = NULL,
                        overwrite = F,
                        ...){
  #Verbose doesn't work

  seatrackR:::checkCon()

  current_user <- DBI::dbGetQuery(con, "SELECT current_user")

  current_roles <- DBI::dbGetQuery(con, paste0("select rolname from pg_user
                                                    join pg_auth_members on (pg_user.usesysid=pg_auth_members.member)
                                                    join pg_roles on (pg_roles.oid=pg_auth_members.roleid)
                                                    where
                                                    pg_user.usename = '", current_user, "'"))
  current_roles <- current_roles[,1]


  if(!("admin" %in% current_roles || "seatrack_writer" %in% current_roles)) stop("Connected user needs to be part of seatrack_writer or admin group")

  if(!tibble::is_tibble(files)) files <- tibble::as_tibble(files)

  fileArchive <- listFileArchive()

  if(any(files$value %in% fileArchive$filesInArchive$filename) & overwrite == F){
    stop(paste("At least one file already exists in the file archive, use overwrite = True to overwrite"))
  } else {


    url <- seatrackR:::.getFtpUrl()

    writeFile <- function(x,
                          url,
                          originFolder = originFolder,
                          ...){

      if(!is.null(originFolder)){
        filename <- paste0(originFolder, "/", x)
      } else {
        filename <- paste(x)
      }

      if(!file.exists(filename)){
        warning(paste("Cannot find file: ", filename ))
        return(paste0("File not uploaded: ", filename))
      } else {

        tmp <- strsplit(url$url, "//")
        getUrl <- paste0(tmp[[1]][1], "//", url$pwd, "@", tmp[[1]][2],"/" , x)

        getHandle <- httr::handle(getUrl)
        filePkg <- httr::upload_file(filename)

        mess  <- lapply(getUrl, seatrackR:::factory(function(x){

          RCurl::ftpUpload(what = filename,
                           to = getUrl,
                           asText = FALSE,
                           use.ssl = TRUE,
                           ssl.verifypeer = FALSE,
                           sslversion = 6L,
                           ...)

        }

        ))


        if(any(grepl("OK", attr(mess[[1]][[1]], "names")))){
          return(paste0("File uploaded: ", x))
        }


      }

    }



    apply(files, 1, function(x) writeFile(x = x, url = url, originFolder = originFolder))

    ##Handle messages like in download, not finished

  }

}

