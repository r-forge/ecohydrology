\name{transmissivity}
\alias{transmissivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transmissivity
}
\description{
Transmissivity
Fraction of direct solar radiation passing through the atmosphere based on the Bristow-Campbell eqn
}
\usage{
transmissivity(Tx, Tn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{Tx}{
maximum daily temperature [C]
}
  \item{Tn}{
minimum daily temperature [C]
}
}
\details{
Can accept either single values or vectors for Tx and Tn.  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Bristow KL, Campbell GS. 1984. 
On the relationship between incoming solar radiation and daily maximum and minimum temperature. 
Agric Forest Meteorol; 31:150-166.
}
\author{
M. Todd Walter, Josephine Archibald
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
