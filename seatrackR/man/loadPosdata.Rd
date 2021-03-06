% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadPosdata.R
\name{loadPosdata}
\alias{loadPosdata}
\title{Load posdata files into R.}
\usage{
loadPosdata(files, originFolder = "../Rawdata")
}
\arguments{
\item{files}{A character vector of posdata files to import. Should not include file endings, i.e. should have a format like "posdata_FULGLA_eynhallow_2014"}
}
\description{
This function loads posdata files from disk into R for further import to database.
}
\examples{
\dontrun{

 files<-c("posdata_FULGLA_eynhallow_2014",
"posdata_FULGLA_eynhallow_2013",
"posdata_FULGLA_eynhallow_2012",
"posdata_FULGLA_eynhallow_2011",
"posdata_FULGLA_eynhallow_2010",
"posdata_FULGLA_eynhallow_2009",
"posdata_FULGLA_eynhallow_2007"
)

toImport <- loadPosdata(files)

summary(toImport)
}



}
