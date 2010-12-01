\name{AtmosphericEmissivity}
\alias{AtmosphericEmissivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title to be replaced
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AtmosphericEmissivity(airtemp, cloudiness)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{airtemp}{
%%     ~~Describe \code{airtemp} here~~
}
  \item{cloudiness}{
%%     ~~Describe \code{cloudiness} here~~
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
function(airtemp,cloudiness){
# the emissivity of the atmsophere [-]

#airtemp: air temperature [C]
#cloudiness: fraction of the sky covered in clouds [-]

return((0.72+0.005*airtemp)*(1-0.84*cloudiness)+0.84*cloudiness)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
