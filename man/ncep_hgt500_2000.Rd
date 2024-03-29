% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdas.R
\docType{data}
\name{ncep_hgt500_2000}
\alias{ncep_hgt500_2000}
\title{Exemplary data for \code{warpGrid.R}}
\format{
A grid object.
}
\source{
\url{https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html} and \url{http://www.meteo.unican.es/udg-tap}.
}
\usage{
data(ncep_hgt500_2000)
}
\description{
This exemplary grid contains monthly mean data of geopotential height at 500hPa from NCEP renalysis, for the year 2000.
This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
log-in is requiered, 
see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
(package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:

\code{loginUDG("username", "pasword")}\cr 
 
\code{ncep_hgt500_2000 <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml", var = "hgt@500", latLim = c(20,90), years = 2000, time="DD", aggr.d="mean", aggr.m="mean")}\cr
}
\examples{
data("ncep_hgt500_2000")
# Plot in longlat projection
visualizeR::spatialPlot(climatology(ncep_hgt500_2000), backdrop.theme = "coastline")
# Plot in polar stereographic projection
grid <- warpGrid(climatology(ncep_hgt500_2000))
library(visualizeR)
l1 <- get(load(paste0(find.package("visualizeR"), "/countries.rda"))) # world coastline
l1 <- sp::spTransform(l1[[2]], CRSobj = attr(grid$xyCoords, "projection"))
visualizeR::spatialPlot(grid, sp.layout = list(list(l1, first = FALSE)))
}
\seealso{
\code{\link[loadeR]{loadGridData}}
}
