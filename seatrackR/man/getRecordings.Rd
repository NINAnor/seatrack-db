% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRecordings.R
\name{getRecordings}
\alias{getRecordings}
\title{Read logger recordings data}
\usage{
getRecordings(
  type = NULL,
  sessionId = NULL,
  individId = NULL,
  colony = NULL,
  species = NULL,
  yearTracked = NULL,
  asTibble = T
)
}
\arguments{
\item{type}{light, temperature, or activity as a character. Default = "light".}

\item{colony}{subset data for a character vector of colony names (International names)}

\item{species}{subset data for a character vector of species}

\item{asTibble}{Boolean. Return result as Tibble instead of lazy query? Tibble is slower, but also here forces the timezone to "UTC".}

\item{session_id}{subset data for a character vector of session ids}

\item{individ_id}{subset data for a character vector of individual ids}

\item{year_tracked}{subset data for a character vector of year_tracked (e.g. 2014_15)}
}
\value{
A Lazy query or optionally a Tibble.
}
\description{
This is a convenience function that reads from the "activity" tables temperature, activity, and light in the schema Recordings
}
\examples{
dontrun{
connectSeatrack(Username = "testreader", Password = "testreader")
getRecordings(type = "temperature",
             colony = "Sklinna")
}
}
