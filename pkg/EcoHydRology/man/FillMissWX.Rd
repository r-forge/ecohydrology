\name{FillMissWX}
\alias{FillMissWX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fill missing weather data with closest GHCN data
}
\description{
A function to fill missing weather data with closest GHCN data
}
\usage{
FillMissWX(declat, declon, StnRadius, date_min, date_max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{declat}{
Latitude of the location. Southern latitudes should be given as negative values.
}
  \item{declon}{
Longitude of the location. Western longitudes should be given as negative values.
}
  \item{StnRadius}{
A numeric vector giving the radius (in kilometers) within which to search for monitors near a location.
}
  \item{date_min}{
A character string giving the earliest date of the daily weather time series that the user would like in the final output. This character string should be formatted as "yyyy-mm-dd". If not specified, the default is to keep all daily data for the queried weather site from the earliest available date.
}
  \item{date_max}{
A character string giving the latest date of the daily weather time series that the user would like in the final output. This character string should be formatted as "yyyy-mm-dd". If not specified, the default is to keep all daily data for the queried weather site through the most current available date.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe of daily weather data that has been filled with the closest GHCN data to minimize the number of NA's.  
 \item{date}{Date of the observation}
 \item{id}{Character string with the weather station site id}
 \item{prcp}{Precipitation, in tenths of mm}
  \item{tmax}{ Maximum temperature, in tenths of degrees Celsius}
   \item{tmin}{ Minimum temperature, in tenths of degrees Celsius} 
   \item{prcpid}{Character string with the weather station site id that fills the missing precipitation data}
    \item{tmaxid}{Character string with the weather station site id that fills the missing maximum temperature data}  
}
\references{
For more information about the data pulled with this function, see:

Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012: An overview of the Global Historical Climatology Network-Daily Database. Journal of Atmospheric and Oceanic Technology, 29, 897-910, doi:10.1175/JTECH-D-11-00103.1.
}
\author{
Roja Kaveh Garna rojakaveh@vt.edu, Zachary M. Easton, Daniel R. Fuka 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Using the latitude and longitude of USGS 04282650 LITTLE OTTER CREEK AT FERRISBURG, VT.
flowgage_id="04282650" 
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date
                       = "2022-01-01")
LOCFer=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,StnRadius=30,date_min="2010-01-01",date_max="2021-01-01")
#View(LOCFer)
plot(LOCFer$date,LOCFer$prcpDis, xlab="Date", ylab = "Distance (km)", ylim=c(0,30))
points(LOCFer$date,LOCFer$tmaxDis-0.5,col="red")
points(LOCFer$date,LOCFer$tminDis+1,col="blue")
legend("topleft",legend = c("prcpDis","tmaxDis-0.5","tminDis+1"),col = c("black","red","blue"),lty = 1:2, cex = 0.7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
