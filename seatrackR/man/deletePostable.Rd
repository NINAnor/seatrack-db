% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deletePostable.R
\name{deletePostable}
\alias{deletePostable}
\title{Delete rows from positions.postable}
\usage{
deletePostable(delFiles)
}
\arguments{
\item{delFiles}{A character vector of posdata files to delete from the postable.}
}
\description{
This is a convenience function that deletes rows from the "positions.postable" table, specified by which posdata file they originated from.
USE THIS WITH CAUTION!
}
\examples{
dontrun{
connectSeatrack(Username = "testreader", Password = "testreader")
delFiles<-c("posdata_FULGLA_eynhallow_2015")
deletePostable(delFiles)

}
}
