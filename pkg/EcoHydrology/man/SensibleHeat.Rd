\name{SensibleHeat}
\alias{SensibleHeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SensibleHeat(surftemp, airtemp, wind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{surftemp}{
%%     ~~Describe \code{surftemp} here~~
}
  \item{airtemp}{
%%     ~~Describe \code{airtemp} here~~
}
  \item{wind}{
%%     ~~Describe \code{wind} here~~
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
function(surftemp,airtemp,wind){
# sensible heat exchange between a surface and the surrounding air [kJ m-2 d-1]

#surftemp: surface temperature [C]
#airtemp: average dailiy air temperature [C]
#wind: average daily windspeed [m/s]

latentht<-2500 #latent heat of vaporization [kJ kg-1]

heatcapacity<-1.25 #approx. heat capacity of air [kJ m-3 C-1]

windfunction<-5.3*(1+wind)

return(86400*heatcapacity*(surftemp-airtemp)*windfunction/latentht)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
