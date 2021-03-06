% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FawnMark.R
\name{FawnMark}
\alias{FawnMark}
\title{Daily Fawn Update}
\usage{
FawnMark(vecpath, ATSUsers, ATSPass, tempdir, ncpu, lookup, from = NA,
  to = NA, subject = NA, SP = NA, progpath = NA, username = NA,
  password = NA, email = "no", ST = TRUE, STUser = NULL,
  STPass = NULL, systype = "Win")
}
\arguments{
\item{vecpath}{path to vectronic data folder}

\item{ATSUsers}{character vector of ATS user names}

\item{ATSPass}{character vector of ATS passwords}

\item{tempdir}{temporary folder for downloading data}

\item{ncpu}{number of CPU cores for multithreaded work}

\item{lookup}{path to lookup table}

\item{ST}{do you have SirTrack data? TRUE/FALSE}

\item{STUser}{SirTrack Username}

\item{STPass}{SirTrack Password}

\item{spp}{Species of animal}
}
\value{
Creates all data needed for fawnmark
}
\description{
Run daily parturition fawn updates data manipulation and plotting
}
\examples{
\donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}

}
\keyword{fawnmark,}
\keyword{prep}
