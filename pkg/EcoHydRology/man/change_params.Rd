\name{change_params}
\alias{change_params}
\docType{data}
\title{
Example dataframe of parameters, ranges, and search strings for optimizing swat2005
%%   ~~ data name/kind ... ~~
}
\description{
Example dataframe of parameters, ranges, and search strings for optimizing swat2005
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(change_params)}
\format{
  A data frame with 15 observations on the following 11 variables.
  \describe{
    \item{\code{filetype}}{a factor with levels \code{0*.gw} \code{0*.mgt} \code{0*.sol} \code{basins.bsn}}
    \item{\code{parameter}}{a factor with levels \code{ALPHA_BF} \code{Ave\. AW} \code{Bulk Density} \code{CN2} \code{Depth} \code{ESCO} \code{GW_DELAY} \code{GWQMN} \code{Ksat\. \(est\.\)} \code{SFTMP} \code{SMFMN} \code{SMFMX} \code{SMTMP} \code{SURLAG} \code{TIMP}}
    \item{\code{alter_type}}{a factor with levels \code{new} \code{percent}}
    \item{\code{min}}{a numeric vector}
    \item{\code{max}}{a numeric vector}
    \item{\code{current}}{a numeric vector}
    \item{\code{multi}}{a logical vector}
    \item{\code{startstr}}{a factor with levels \code{} \code{Ave. AW Incl. Rock Frag  :} \code{Bulk Density Moist [g/cc]:} \code{Depth                [mm]:} \code{Ksat. (est.)      [mm/hr]:}}
    \item{\code{endstr}}{a factor with levels \code{} \code{    | ALPHA_BF} \code{    | CN2} \code{    | ESCO} \code{    | GW_DELAY} \code{    | GWQMN} \code{    | SFTMP} \code{    | SMFMN} \code{    | SMFMX} \code{    | SMTMP} \code{    | SURLAG} \code{    | TIMP}}
    \item{\code{frformat}}{a factor with levels \code{} \code{A27,10f12}}
    \item{\code{fwformat}}{a factor with levels \code{F16.4} \code{F20}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(change_params)
## maybe str(change_params) ; plot(change_params) ...
}
\keyword{datasets}
