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
                        verbose = F){
  checkCon()

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
                        verbose = verbose){

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

    mess  <- lapply(getUrl, factory(function(x){
      httr::with_config(httr::config(ssl_verifypeer = F,
                                     ssl_verifyhost = F,
                                     use_ssl = T,
                                     upload = T,
                                     filetime = F,
                                     sslversion = 6L),
                        httr::PUT(getUrl, body = filePkg))
      }))


    if(any(grepl("File successfully transferred", mess[[1]]$warn))){
      return(paste0("File uploaded: ", x))
    }


    }

  }

    # with_config(httr::config(ssl_verifypeer = F,
    #                           ssl_verifyhost = F,
    #                           use_ssl = T,
    #                          upload = T,
    #                          filetime = F), PUT(getUrl, body = filePkg))
    #
  #
  #   with_config(httr::config(ssl_verifypeer = F,
  #                            ssl_verifyhost = F,
  #                            use_ssl = T,
  #                            upload = T,
  #                            ftp_use_epsv = TRUE,
  #                            filetime = F), PUT(
  #     url = getUrl,
  #     body = upload_file(
  #       path =  path.expand("temp/test4.txt"),
  #       type = 'text/csv'),
  #     verbose()
  #   )
  #   )
  #
  #   ##PUT to existing file works, but not to new filename MDTM test4.txt
  #
  #   toPut <- form_file(filename)
  #   put_files <- curl::new_handle()
  #
  #   curl::handle_setopt(put_files,
  #                       ftp_use_epsv = TRUE,
  #                       upload = TRUE,
  #                       use_ssl = T,
  #                       ssl_verifyhost = F,
  #                       ssl_verifypeer = F,
  #                       upload = T
  #                     )
  #
  #
  #
  #   con <- curl::curl(url = getUrl, toPut, open = "r", handle = put_files)
  #
  #   filesInStorage <- readLines(con)
  #   close(con)
  #
  #   ##LOGIN WORKS BUT SOMEHOW retr fails
  #
  #   a <- RCurl::ftpUpload(what = filename,
  #                         to = getUrl,
  #                         ftpsslauth = T,
  #                         ftp.ssl = T,
  #                         ssl.verifypeer = F,
  #                         ssl.verifyhost = F,
  #                         verbose = verbose)
  #
  #   if(a == 0){
  #     return(paste0("File uploaded: ", filename))
  #   } else
  #   return(a)
  # }

  apply(files, 1, function(x) writeFile(x = x, url = url, originFolder = originFolder))

  ##Handle messages like in download

  }

}
