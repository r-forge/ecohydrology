\name{SatVapPresSlope}
\alias{SatVapPresSlope}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Slope of the relationship between Saturation Vapor Pressure and Temperature
}
\description{
Calculates the slope of the Saturation Vapor Pressure vs T (kPa/K)
}
\usage{
SatVapPresSlope(temp_C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp_C}{
Air temperature in degrees C
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
slope of the Saturation Vapor Pressure vs T (kPa/K)
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Dingman, S. Lawrence. 2002. Physical Hydrology.  Waveland Press Inc.
}
\author{
Josephine Archibald
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function (temp_C) 
{
    (2508.3/(temp_C + 237.3)^2) * exp(17.3 * temp_C/(temp_C + 
        237.3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
