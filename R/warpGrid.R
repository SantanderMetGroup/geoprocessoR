#     warpGrid.R Warp grid to allow plotting in a different projection
#
#     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Grid warping
#' @description Warp grid to allow plotting in a different projection.
#' @param data A C4R grid (or multimember C4R grid) object, or climatology C4R grid.
#' @param original.CRS character as passed to function \code{\link{CRS}} with the original projection. 
#' Default to longlat projection (\code{"+init=epsg:4326"}).
#' @param new.CRS character string, as passed to function \code{\link{CRS}}, specifying the target projection. 
#' Default to polar stereographic projection (\code{"+init=epsg:3995"}).
#' @param int.method Resampling method. Default to \code{"bilinear"}. See details.
#' 
#' @return Warped grid with the structure of a C4R grid.
#' 
#' @details 
#' This function is a wrapper of the gdal warping capabilities via gdalwarp.  
#' 
#'  \strong{int.method}
#'  
#'  By default bilinear interpolation is applied to get a complete grid in the target projection. Other options are \code{"near"}, \code{"cubic"},
#'   \code{"cubicspline"} etc., passed to the argument \code{r} in \code{gdalUtils::gdalwarp}.

#' @export
#' @importFrom sp spplot spTransform CRS 
#' @importFrom gdalUtils gdalwarp
#' @importFrom rgdal writeGDAL readGDAL
#' @import transformeR
#' @author A. Casanueva, J. Bedia, M. Iturbide
#' @examples
#' library(climate4R.datasets)
#' data(ncep_hgt500_2000)
#' grid <- warpGrid(climatology(ncep_hgt500_2000))
#' # Example of application: plot in polar stereographic projection
#' library(visualizeR)
#' l1 <- get(load(paste0(find.package("visualizeR"), "/countries.rda"))) # world coastline
#' l1 <- sp::spTransform(l1[[2]], CRSobj = attr(grid$xyCoords, "projection"))
#' visualizeR::spatialPlot(grid, sp.layout = list(list(l1, first = FALSE)))

warpGrid <- function(data,
                     original.CRS = "+init=epsg:4326",
                     new.CRS = "+init=epsg:3995", 
                     int.method = "bilinear") {
  
  # *** Check for members ***
  nmem <- getShape(data, "member")
  member <- ifelse((nmem == 1 | is.na(nmem)), FALSE, TRUE)
  
  # *** CONVERT GRID TO A SpatialPointsDataFrame ***
  pattern <- transformeR::grid2sp(data)
  
  # *** WRITE A GDAL GRID MAP ***
  outf <- tempfile(fileext = ".tif")
  suppressWarnings(
    rgdal::writeGDAL(pattern, fname = outf, drivername = "GTiff", mvFlag = "NA")
  )
  
  # *** IMAGE RE-PROJECTION ***
  newf <- tempfile(fileext = ".tif")
  suppressMessages(
    gdalUtils::gdalwarp(srcfile = outf,
                        s_srs = original.CRS,
                        t_srs = new.CRS,
                        dstfile = newf,
                        r = int.method)
  )
  # *** READ NEW IMAGE ***
  n <- rgdal::readGDAL(newf)
  outf <- newf <- NULL
  
  # *** sp2grid ***
  start <- getRefDates(data, which = "start")
  end <- getRefDates(data, which = "end")
  
  grid <- transformeR::sgdf2clim(sp = n,
                                 varName = getVarNames(data),
                                 level = getGridVerticalLevels(data),
                                 dates = list(start = start, end = end),
                                 season = getSeason(data))
  return(grid)
}

