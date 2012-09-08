\name{extract.Cu}
\alias{extract.Cu}
\title{Extracts Cu SMI capture histories and covariates
Extracts data from ACCESS databaseand constructs the relevant capture histories from the
tagging(Cutags) and resighting (Cutag Live Resights) tables. It constructs all the
queries that used to be done in ACCESS.}
\usage{
  extract.Cu(dir = NULL, begin = 615, end = 831)
}
\arguments{
  \item{dir}{Directory containing Cu database; NULL uses
  default location}

  \item{begin}{month-day at beginning of resight period
  (515 = May 15)}

  \item{end}{month-day at end of resight period (831 =
  August 31)}
}
\value{
  dataframe containing following fields \item{ch}{capture
  history; character string} \item{cohort}{year branded;
  factor variable} \item{sex}{either M or F; factor
  variable} \item{weight}{weight (kg) at time of branding}
  \item{ID}{tag identifier} \item{tagtype}{type of tag
  applied on left/right } \item{numtags}{number of tags
  applied} \item{repro}{sequence of fields named reproyyyy
  with values 0/1; it is 1 if seen as reproductive in a
  year prior to yyyy} \item{td}{sequence of fields named
  tdyyyy with values 0/1; it is 1 if seen in year yyyy-1}
  \item{recap}{0 if never resighted and 1 otherwise}
  \item{TotalTimesResighted}{number of years resighted}
}
\description{
  Attaches directly to ACCESS database and extracts initial
  brand and all resighting information.  Excludes any fur
  seals with missing sex or initial weight. Also, excludes
  any not tagged in the fall or any from Castle Rock.
  Restricts resightings to those seen between \code{begin}
  and \code{end} inclusive of those dates.
}
\examples{
cudata=extract.Cu()
}
\author{
  Jeff Laake
}

