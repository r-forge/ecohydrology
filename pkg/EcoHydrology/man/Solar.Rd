\name{Solar}
\alias{Solar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Solar(lat, Jday, Tx, Tn, albedo, forest, slope, aspect)
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
function(lat,Jday,Tx,Tn,albedo,forest,slope,aspect){
# solar radiation at the ground surface [kJ m-2 d-1]

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#Tx: maximum daily temperature [C]
#Tn: minimum daily temperature [C]
#albedo: surface albedo or reflectivity [-]
#forest: forest or vegeation cover [-]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return((1-albedo)*(1-forest)*transmissivity(lat,Jday,Tx,Tn)*PotentialSolar(lat,Jday)*slopefactor(lat,Jday,slope,aspect))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
