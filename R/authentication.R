#' Set up SEATRACK DB credentials for this project
#'
#' This function will store credentials to access the seatrack database in a .Renviron file,
#' meaning they can be automatically be used when connecting to the seatrack database.
#' If the project is git controlled, .Renviron will be added to .gitignore to avoid credentials being leaked.
#' This function should only have to be called once, when setting up a new project.
#' While the option is there to directly put your credentials in as arguments, do NOT put it in your script with the password in plain text, as this defeats the purpose.
#'
#' @param user_name Username to access the seatrack database. If not provided, user will be prompted.
#' @param password Password to access the seatrack database If not provided, user will be prompted.
#'
#' @example \dontrun{
#'  set_credentials_renviron() # will prompt
#'  set_credentials_renviron("foo","bar") #only do this in the console
#' }
#' @export
set_credentials_renviron <- function(user_name = NULL, password = NULL) {
    if (is.null(user_name)) {
        user_name <- readline("Enter your username:")
    }
    if (is.null(password)) {
        password <- getPass::getPass(msg = "Enter your password:")
    }

    if (file.exists(".Renviron")) {
        environ_lines <- readLines(".Renviron")
    } else {
        environ_lines <- c()
    }

    environ_lines <- c(environ_lines, paste0("SEATRACK_DB_USER = '", user_name, "'"), paste0("SEATRACK_DB_PWD = '", password, "'"))

    writeLines(unique(environ_lines), ".Renviron")
    print("Wrote credentials to .Renviron")

    # immediately set the env var so R does not have to be restarted
    Sys.setenv(SEATRACK_DB_USER = user_name)
    Sys.setenv(SEATRACK_DB_PWD = password)

    if (file.exists(".gitignore")) {
        git_ignore_lines <- readLines(".gitignore")
    } else {
        if (git2r::in_repository(".")) {
            # add git ignore file
            git_ignore_lines <- c()
        } else {
            # Exit early
            return()
        }
    }

    if (!".Renviron" %in% git_ignore_lines) {
        print("Adding .Renviron to .gitignore")
        git_ignore_lines <- c(git_ignore_lines, )
        writeLines(unique(git_ignore_lines), ".gitignore")
    }

}
