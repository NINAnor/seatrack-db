#' Internal functions
#'
#'
#'


upstartVersion <- function(){

  pkg = "seatrackR"

  installed_version <- tryCatch(packageVersion(gsub(".*/",
                                                    "", pkg)), error = function(e) NA)

  url <- "https://raw.githubusercontent.com/NINAnor/seatrack-db/master/seatrackR/DESCRIPTION"
  x <- readLines(url)

  remote_version <- gsub("(Version: )(.*)", "\\2", grep("Version:", x, value = T))


  res <- list(package = pkg, installed_version = installed_version,
              latest_version = remote_version, up_to_date = NA)

  if (remote_version > installed_version) {
    msg <- paste("##", pkg, "is out of date, latest version is",
                 remote_version)
    message(msg)
    res$up_to_date <- FALSE


  }
}

#upstartVersion

checkCon <- function() {if(!exists("con")){ stop("No connection, run connectSeatrack()")} else{
  if(class(con)!= "PqConnection"){ stop("\"con\" is not of class \"PqConnection\". Have you run connectSeatrack()?")}
  if(!DBI::dbIsValid(con)) { stop("No connection, run connectSeatrack()")}
  }
}


compareNA <- function(v1,v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

passEnv <- new.env()


.getFtpUrl <- function(){
  checkCon()

  password = get(".pass", envir = passEnv)
  current_user <- DBI::dbGetQuery(con, "SELECT current_user")

  pwd <- paste0(password, current_user)

  pwd <- paste0(current_user, ":", pwd)

  url <- "ftp://seatrack.nina.no"

  out <- list("url" = url,
              "pwd" = pwd)
  return(out)

}

# .getFtpUrl <- function(write = F){
#   checkCon()
#
#   if(write == T){
#     current_user <- DBI::dbGetQuery(con, "SELECT current_user")
#
#     current_roles <- DBI::dbGetQuery(con, paste0("select rolname from pg_user
#                                                  join pg_auth_members on (pg_user.usesysid=pg_auth_members.member)
#                                                  join pg_roles on (pg_roles.oid=pg_auth_members.roleid)
#                                                  where
#                                                  pg_user.usename = '", current_user, "'"))
#
#   if(!("admin" %in% current_roles)) stop("Connected user needs to be part of admin group")
#
#   pwd <- DBI::dbGetQuery(con, "SELECT pwd from restricted.write WHERE name = 'ftp.nina.no'")
#
#   pwd <- paste0("ftp.nina.no|ftpintern:", pwd)
#
#   url <- "ftp://ftp.nina.no/Download/Seatrack/"
#
#   out <- list("url" = url,
#               "pwd" = pwd)
#
#   return(out)
#   }
#
#   if(write == F) {
#
#     pwd <- DBI::dbGetQuery(con, "SELECT pwd from restricted.read WHERE name = 'ftp.nina.no'")
#
#     pwd <- paste0("ftp.nina.no|ftpekstern:", pwd)
#
#     url <- "ftp://ftp.nina.no/Download/Seatrack/"
#
#     out <- list("url" = url,
#                 "pwd" = pwd)
#     return(out)
#   }
#
# }


reakHavoc <- function(){

  checkCon()

  answer <- menu(c("Yes (1)", "No (2)"), title ="You are about to delete all logger records!!! Are you sure?")

  havoc1 <- "TRUNCATE TABLE loggers.logger_info RESTART IDENTITY CASCADE;"
  havoc2 <- "TRUNCATE TABLE individuals.individ_info RESTART IDENTITY CASCADE;"



  if(answer == 1){
    dbSendStatement(con, havoc1)
    dbSendStatement(con, havoc2)
    return("Things are gone, database should be clean!")
    } else return("Nothing")

}


##Error handling from stackoverflow user Martin Morgan, Q: 4948361

factory <- function(fun)
  function(...) {
    warn <- err <- mess <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m){
        mess <<- append(mess, conditionMessage(m))
      })
    list(res, warn=warn, err=err, mess = mess)
  }

.has <- function(x, what)
  !sapply(lapply(x, "[[", what), is.null)
hasWarning <- function(x) .has(x, "warn")
hasError <- function(x) .has(x, "err")
isClean <- function(x) !(hasError(x) | hasWarning(x))
value <- function(x) sapply(x, "[[", 1)
cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)

