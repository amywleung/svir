% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svi_calc.R
\name{svi_calc}
\alias{svi_calc}
\title{svi_calc}
\usage{
svi_calc(con, res)
}
\arguments{
\item{con}{A PostGIS database connection}

\item{res}{The result from an ST_Intersects() of PostGIS spatial tables}
}
\value{
A SpatialPolygons Data Frame object resulting from PostgreSQL connection \code{con}
and the resulting object from PostGIS function ST_Intersects() \code{res}
}
\description{
Calculates the CDC/ATSDR formulated Social Vulnerability Index (SVI) using ACS 2010-2014 5-year
estimates. User must specify a PostGIS database connection and the ST_Intersects() result as
inputs.
}
