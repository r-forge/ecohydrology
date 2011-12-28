\name{SnowMelt}
\alias{SnowMelt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Snow Melt Calculator
}
\description{
Calculates snow pack accumulation and melt
}
\usage{
SnowMelt(Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope = 0, aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25, SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Date}{
Vector of dates in this format: "%Y-%m-%d"
}
  \item{precip_mm}{
Precipitation in mm
}
  \item{Tmax_C}{
Daily maximum temperature (degrees C)
}
  \item{Tmin_C}{
Daily minimum temperature (degrees C)
}
  \item{lat_deg}{
Degrees latitude
}
  \item{slope}{
Overall slope of area of interest
}
  \item{aspect}{
Aspect of the area of interest
}
  \item{tempHt}{
height of temperature measurements (m)
}
  \item{windHt}{
height of wind measurements (m)
}
  \item{groundAlbedo}{
Ground Albedo (-)
}
  \item{SurfEmissiv}{
Surface Emissivity (-)
}
  \item{windSp}{
Wind speed - either a vector of measured values or a single value of average wind speed for the site (m/s)
}
  \item{forest}{
Forest cover - use this only when determining snowmelt under a canopy (-)
}
  \item{startingSnowDepth_m}{
The depth of the snow pack initially (m)
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
Walter, M.T. , E.S. Brooks, D.K. McCool, L.G. King, M. Molnau, J. Boll. 2005. 
Process-based snowmelt modeling: Does it require more input data than temperature-index modeling? 
Journal of Hydrology 300(1-4): 65-75
}
\author{
Josephine Archibald, M. Todd Walter
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
