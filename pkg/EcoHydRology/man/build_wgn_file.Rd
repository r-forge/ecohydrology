\name{FillMissWX}
\alias{FillMissWX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fill missing weather data.
}
\description{
A function to fill missing weather data with closest GHCN data based on inverse distance weighting method. This function generates a dataframe which contains filled weather data 
  including Precipitation (P (mm)), Maximum temperature (MaxTemp (c)), Minimum temperature (MinTemp (c)). Three diffrent filling methods can be selected by user which are inverse
  distance weighting (method="IDW"), inverse distance and elevation weighting (method="IDEW"), and closeset GHCN weather data (method="closest").
}
\usage{
build_wgn_file(metdata_df=WXData,declat=flowgage$declat,declon=flowgage$declon)
FillMissWX(declat, declon, StnRadius, minstns, date_min, date_max, targElev, method, alfa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata_df}{
A list in the form returned by FillMissWX() 
}
  \item{declat}{
Latitude of the location. Southern latitudes should be given as negative values.
}
  \item{declon}{
Longitude of the location. Western longitudes should be given as negative values.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Creates a SWAT formatted .wgn weather generator file for a given subbasin ID. See the 
.wgn file documentation on the SWAT page: http://swatmodel.tamu.edu for current format 
and descriptions.
}
\references{
For more information about the data pulled with this function, see:
http://swatmodel.tamu.edu
}
\author{
Daniel R. Fuka drfuka@vt.edu, Roja Kaveh Garna rojakaveh@vt.edu, Zachary M. Easton
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Using the latitude and longitude of USGS 04282650 
# LITTLE OTTER CREEK AT FERRISBURG, VT.
\dontrun{
flowgage_id="04282650" 
flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",
                       end_date = "2022-01-01")
LOCFerIDEW=FillMissWX(declat=flowgage$declat, declon=flowgage$declon,
                          StnRadius=30,minstns=10,date_min="2010-01-01",
                          date_max="2021-01-01",targElev=1,
                          method = "IDEW",alfa)
build_wgn_file(metdata_df=LOCFerIDEW,declat=flowgage$declat,declon=flowgage$declon)

}
}
