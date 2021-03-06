% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFileArchiveSummary.R
\name{getFileArchiveSummary}
\alias{getFileArchiveSummary}
\title{Retrieve summary info on the registered raw-files}
\usage{
getFileArchiveSummary(colony = NULL, year = NULL)
}
\value{
Data frame.
}
\description{
This is a convenience function that pulls together various info on the files in the loggers.file_archive table and other tables
}
\examples{
dontrun{
seatrackConnect(Username = "testreader", Password = "testreader")
fileArchive <- getFileArchive()
}
}
