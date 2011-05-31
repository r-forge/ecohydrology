\name{readSWAT}
\alias{readSWAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readSWAT(outfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
function(outfile){

if (outfile=="sub"){
   varformat="x6,a4,1x,a8,1x,a4,a10,30a10"
   dataformat="x6,i4,1x,i8,1x,i4,f10,30f10"
  } else if (outfile=="rch"){
   varformat="x6,a4,1x,a8,1x,a5,30a12"
   dataformat="x6,i4,1x,i8,1x,i5,30f12"
  } else { print ("You need to add your file type to this function if it is not output.sub or output.rch")}
  print(varformat)
  print(dataformat)
  vfrformat = unlist(strsplit(as.character(varformat), ","))
  dfrformat = unlist(strsplit(as.character(dataformat), ","))
  outvars=read.fortran(paste("output.",outfile,sep=""),vfrformat,skip=8,nrows=1)
  outdata=read.fortran(paste("output.",outfile,sep=""),dfrformat,skip=9,col.names=outvars)
  return(outdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
