\name{GPSToDistance}
\alias{GPSToDistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert GPS coordinates to rectangular coordinates.
}
\description{
This function takes a series of two vectors; lats (latitudes) and lons
(longitude), which should be numerical latitute/longitude pairs in
numerical/decimal values. It finds the mean value of each dimension to
center the  values, and then converts the lat/long values into meters
from the center of the data, using a simple curvature model. It may not
be robust to edge effects when values cross -180/+180 boundary.

This is not currently used directly by any cost functions but may be used to either
convert GPS for local areas, or to adapt a new cost function.

}
\usage{
GPSToDistance(longs,lats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lons}{
A vector of numerical longitude values.
}
\item{lats}{
  A vector of numerical latitude values.
}
\value{
returns a data frame of  scalar values indicating rectangular coordinates.
}
\references{
 See the following URLs for reference:
 http://fmepedia.safe.com/articles/How_To/Calculating-accurate-length-in-meters-for-lat-long-coordinate-systems
 http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.HTM
 http://www-pord.ucsd.edu/~matlab/coord.htm

See  also Mueller et al., (2015). \url{https://sites.google.com/a/mtu.edu/mapping/}
}
\author{
Shane T. Mueller and Brandon Perelman
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


