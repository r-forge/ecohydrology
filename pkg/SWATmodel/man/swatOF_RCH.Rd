\name{swat_objective_function}
\alias{swat_objective_function}
\title{
  Updated objective function, NSE of flowgage$flowdata(CMD) vs output.rch 
}
\description{
  modified from swat_objective_function_rch
  Presumes faster readSWAT using data.table
  Presumes flowgage as previous swat_objective_function_rch, with flowgage$flowdata in CMD
}
\usage{
  swatOF_RCH(x, calib_params, flowgage, rch)
}
\arguments{
  \item{x}{
    \code{x} A vector of adjustment factors to be fed into alter_params function, as passed by calibration function
  }
  \item{calib_params}{
    Data frame of parameter info, e.g., as selected from change_params
    load(paste(path.package("EcoHydRology"), "data/change_params.rda", sep = "/")) 
  }
  \item{flowgage}{
    Object with observed flows formatted as returned by EcoHydRology::get_usgs_gage()
  }
  \item{rch}{
    The SWAT RCH number of the flow against which to calibrate
  }
}
\author{
  Daniel Auerbach, Daniel Fuka
}
\examples{
  \dontrun{
    Customize to your needs...
  }
}
