\name{get_grdc_gage}
\alias{get_grdc_gage}
\title{
A function to grab daily stream flow measurements from GRDC Daily files
}
\description{
A function to grab daily stream flow measurements from a GRDC Daily Flow file and makes a list metadata + data object similar to EcoHydrology::get_grdc_gage.
}
\usage{
get_grdc_gage(filename=grdcfilename){  
}
\arguments{
  \item{filename}{
  \code{flowgage_id} Gage is a TEXT/String rather than numeric or the query will fail on gages with leading 0s
}
}
\value{
\item{area}{The area above the gage [km2]}
\item{declat}{Decimal Lat}
\item{declon}{Decimal Lon}
\item{flowdata}{Dataframe with the historical flow data - flow is in cubic meters per day}
}
\author{
Daniel R Fuka
}

\examples{

grdcfilename="1577050_Q_Day.Cmd.txt"
flowgage=get_grdc_gage(filename=grdcfilename)

## The function is currently defined as
}
