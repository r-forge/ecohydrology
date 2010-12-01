\name{slopefactor}
\alias{slopefactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
slopefactor(lat, Jday, slope, aspect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{Jday}{
%%     ~~Describe \code{Jday} here~~
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
function(lat,Jday,slope,aspect){
# slopefactor: adjusts solar radiation for land slope and aspect relative to the sun, 1=level ground

#lat: latitdue [rad]
#Jday: Julian date or day of the year [day]
#slope: slope of the ground [rad]
#aspect: ground aspect [rad from north]

return(cos(slope)-sin(slope)*1/tan(solarangle(lat,Jday))*cos(aspect-solaraspect(lat,Jday)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
