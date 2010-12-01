\name{EnvirEnergy}
\alias{EnvirEnergy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EnvirEnergy(lat, Jday, Tx, Tn, wind, relativehumidity, cloudiness, albedo, forest, slope, aspect, surftemp, surfemissivity)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{Jday}{
%%     ~~Describe \code{Jday} here~~
}
  \item{Tx}{
%%     ~~Describe \code{Tx} here~~
}
  \item{Tn}{
%%     ~~Describe \code{Tn} here~~
}
  \item{wind}{
%%     ~~Describe \code{wind} here~~
}
  \item{relativehumidity}{
%%     ~~Describe \code{relativehumidity} here~~
}
  \item{cloudiness}{
%%     ~~Describe \code{cloudiness} here~~
}
  \item{albedo}{
%%     ~~Describe \code{albedo} here~~
}
  \item{forest}{
%%     ~~Describe \code{forest} here~~
}
  \item{slope}{
%%     ~~Describe \code{slope} here~~
}
  \item{aspect}{
%%     ~~Describe \code{aspect} here~~
}
  \item{surftemp}{
%%     ~~Describe \code{surftemp} here~~
}
  \item{surfemissivity}{
%%     ~~Describe \code{surfemissivity} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(lat,Jday,Tx,Tn,wind,relativehumidity,cloudiness,albedo,forest,slope,aspect,surftemp,surfemissivity){
# the total energy exchange between the surface and the surrounding air

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
#wind: average daily windspeed [m/s]
#relativehumidity: relative humidity; if negative, air vapor density will be approximated [-]
#cloudiness: fraction of the sky covered in clouds,if negative, cloudiness will be approximated [-]
#albedo: surface albedo or reflectivity [-]
#forest: forest or vegeation cover [-]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]
#surftemp: surface temperature [C]
#surfemissivity: [-]

if(cloudiness<0){cloudiness<-EstCloudiness(lat,Jday,Tx,Tn)}

airtemp<-(Tx+Tn)/2 #average daily air temperature [C]

return(Solar(lat,Jday,Tx,Tn,albedo,forest,slope,aspect)+Longwave(AtmosphericEmissivity(airtemp,cloudiness),airtemp)-Longwave(surfemissivity,surftemp)+SensibleHeat(surftemp,airtemp,wind)+EvapHeat(surftemp,airtemp,relativehumidity,Tn,wind)+RainHeat(airtemp,rain)+GroundHeat())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
