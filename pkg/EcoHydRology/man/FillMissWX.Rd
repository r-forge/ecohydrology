\name{FillMissWX}
\alias{FillMissWX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fill missing weather data.
}
\description{
A function to fill missing weather data with closest GHCN data based on inverse distance weighting method. This function generates a dataframe which contains filled weather data 
  including Precipitation (P (mm)), Maximum temperature (MaxTemp (c)), Minimum temperature (MinTemp (c)). Three diffrent filling methods can be selected by user which are inverse
  distance weighting (method="IDW"), inverse distance and elevation weighting (method="IDEW"), and closeset GHCN weather data (method="closest").
}
\usage{
FillMissWX(declat, declon, StnRadius, minstns, date_min, date_max, targElev, method, alfa)
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
   \item{minstns}{
A numeric vector giving the minimum number of monitors.
}
  \item{date_min}{
A character string giving the earliest date of the daily weather time series that the user would like in the final output. This character string should be formatted as "yyyy-mm-dd". If not specified, the default is to keep all daily data for the queried weather site from the earliest available date.
}
  \item{date_max}{
A character string giving the latest date of the daily weather time series that the user would like in the final output. This character string should be formatted as "yyyy-mm-dd". If not specified, the default is to keep all daily data for the queried weather site through the most current available date.
}
   \item{targElev}{
A numeric vector giving the elevation of target location in IDEW method.
}
   \item{method}{
A vector of character strings representing the method that has been used to fill weather data. The value should be any subset of the values c("IDW", "closest", "IDEW").
}
  \item{alfa}{
The power in IDW equation, if method=“IDW”. The default value is 2.
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
# Using the latitude and longitude of USGS 04282650 
# LITTLE OTTER CREEK AT FERRISBURG, VT.
\dontrun{
flowgage_id="04282650" 
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",
                       end_date = "2022-01-01")
LOCFerIDW=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                          StnRadius=30,minstns=10,date_min="2010-01-01",
                          date_max="2021-01-01",targElev=1,
                    method = "IDW",alfa)
LOCFerIDEW=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                          StnRadius=30,minstns=10,date_min="2010-01-01",
                          date_max="2021-01-01",targElev=1,
                    method = "IDEW",alfa)
LOCFerClosest=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                          StnRadius=30,minstns=10,date_min="2010-01-01",
                          date_max="2021-01-01",targElev=1,
                         method = "closest",alfa)
View(LOCFerIDW)
View(LOCFerIDEW)
View(LOCFerClosest)

}
}
