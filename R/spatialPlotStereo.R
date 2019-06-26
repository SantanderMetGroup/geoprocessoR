#     spatialPlotStereo.R Plot spatial map with polar stereographic projection
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

#' @title Spatial map with polar stereographic projection.
#' @description Plot spatial map with polar stereographic projection.
#' @param data A climatology of a grid (gridded or station dataset) or multimember grid object.
#' @param original.CRS character as passed to function \code{\link{CRS}} with the original projection. Default: longlat projection.
#' @param new.CRS character as passed to function \code{\link{CRS}} with the target projection. Default: polar stereographic projection.
#' @param int.method character with the interpolation method in the new projection. Default: "bilinear". See details.
#' @param backdrop.theme Reference geographical lines to be added to the plot. Default: "none". See details. 
#' @param set.min Numeric value indicating an absolute minimum value (default to minimum value). All grid values below this are mapped to \code{set.min}. See details.
#' @param set.max Same as \code{set.min} argument, but to force a ceiling. 
#' @param color.theme A character string indicating the color theme to use in the map. 
#' Valid values are those available in the \code{\link{RColorBrewer}} themes. Additionally,
#' the \code{"jet.colors"} palette can be used (the rainbow colors, in general not advised, though),
#'  for backwards compatibility. Default to the diverging, colorblind-friendly \code{"RdYlBu"} palette.
#'  NOTE: the \code{color.theme} argument will be overriden if the \code{col.regions} option from \code{spplot} is used.
#' @param rev.colors Logical. Should the chosen color theme be reversed? (default to FALSE, leaving the palette “as is”).
#' @param ... Further arguments passed to \code{spplot}.
#' 
#' @return A spatial plot using polar stereographic projection. As spplot, \code{spatialPlotStereo} returns a lattice plot of class \dQuote{trellis}. 
#' 
#' @details The function applies the \code{\link[sp]{spplot}} method after conversion of the climatological map(s) to a
#'  \code{SpatialGridDataFrame}.
#'  
#'  The \code{set.min} and \code{set.max} options are useful in order to preserve adequate ranges for map representation,
#'   avoiding the influence of extreme values. Note that this is different than setting a range of values with an 
#'   interval using the \code{at} argument. The latter choice, that overrides \code{set.min} and \code{set.max},
#'    leaves blank grid points for outlying values.
#'    
#' \strong{Backdrop theme}
#'  
#'  Current implemented options are \code{"none"} and \code{"coastline"}, which contains
#'  a simplied vector theme delineating the world coastlines with the stereographic projection. Any other themes can be introduced
#'  by the user using the \code{sp.layout} options in \code{spplot}.
#' 
#' \strong{Controlling graphical parameters}
#'  
#'  Many different aspects of the map can be controlled passing the relevant arguments to 
#'  \code{spplot}. Fine control of graphical parameters for the trellis display can
#'  be also controlled using \code{\link[lattice]{trellis.par.set}}.
#'  
#'  \strong{int.method}
#'  
#'  By default bilinear interpolation is applied to get a complete grid in the target projection. Other options are "near", "cubic", "cubicspline", etc., corresponding to the argument \code{r} in \code{ gdalUtils::gdalwarp}.

#' @export
#' @import visualizeR
#' @importFrom sp spplot spTransform CRS 
#' @importFrom gdalUtils gdalwarp
#' @importFrom rgdal writeGDAL readGDAL
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @author A. Casanueva, J. Bedia
#' @examples 
#' data(ncep_hgt500_2000)
#' spatialPlotStereo(climatology(ncep_hgt500_2000), backdrop.theme = "coastline")


spatialPlotStereo <- function(data, original.CRS="+init=epsg:4326", new.CRS="+init=epsg:3995", 
                              int.method="bilinear",
                              backdrop.theme = "none", 
                              set.min=floor(min(data$Data)), set.max=ceiling(max(data$Data)),
                              color.theme = "RdYlBu", rev.colors = FALSE, ...){

  arg.list <- list(...)
  
  # *** SET DATA LIMITS ***
  if (!is.null(set.min) && !is.numeric(set.min)) stop("Invalid 'set.min' value")
  if (!is.null(set.max) && !is.numeric(set.max)) stop("Invalid 'set.max' value")
  
  
  # *** COLORBAR *** 
  if (is.null(arg.list[["col.regions"]])) {
    if (color.theme == "jet.colors") {
      coltheme <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    } else {
      coltheme <- brewer.pal(name = color.theme, n = brewer.pal.info[color.theme, ]$maxcolors)
    }
    if (isTRUE(rev.colors)) coltheme <- rev(coltheme)
    colorpal <- colorRampPalette(coltheme)
    arg.list[["col.regions"]] <- colorpal(101)
  }
  
  # *** CONVERT GRID TO A SpatialPointsDataFrame ***
  pattern <- visualizeR::clim2sgdf(data, set.min = set.min, set.max = set.max)
  
  # *** WRITE A GDAL GRID MAP ***
  outf <- tempfile(fileext = ".tif")
  rgdal::writeGDAL(pattern, fname = outf, drivername = "GTiff", mvFlag = "NA")
  
  # *** IMAGE RE-PROJECTION ***
  newf <- tempfile(fileext = ".tif")
  gdalUtils::gdalwarp(srcfile = outf,
                    s_srs = original.CRS,
                    t_srs = new.CRS, dstfile = newf,
                    r=int.method)
  
  # *** READ NEW IMAGE ***
  n <- rgdal::readGDAL(newf)
  arg.list[["obj"]] <- n

  # *** PLOT DATA ***
  if(backdrop.theme=="coastline"){
  
  # *** CONVERT MAP TO SP ***
  m = maps2sp(c(-180,180),ylim=range(data$xyCoords$y)) 
  m.proj <-  c(spTransform(m, CRS(new.CRS)), first=FALSE)
  if (is.null(arg.list[["sp.layout"]])) {
    arg.list[["sp.layout"]] <- m.proj
  } else {
    arg.list[["sp.layout"]][[length(arg.list[["sp.layout"]]) + 1]] <- m.proj
  }
  } 
  rm(outf, newf)
  
  do.call("spplot", arg.list)

}


# maps2sp.R Function to slice and dice a map in longlat coordinates and convert it to an sp() object
#' @title Transform map to sp object.
#' @description Function to slice and dice a map in longlat coordinates and convert it to an sp() object.
#' @param xlim 2-element vector with longitude limits.
#' @param ylim 2-element vector with latitude limits.
#' @param l.out numeric. Length of latitudes and longitudes vector to plot.
#' @param proj character as passed to function \code{\link{CRS}} with the projection. Default: longlat projection.

#' @return A sp object.

#' @export
#' @importFrom sp SpatialPolygons Polygons CRS 
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom rgeos gIntersection
#' @examples 
#' m <-maps2sp(xlim=c(-20,40), ylim=c(20,70))
#' sp::spplot(m)
#' 

maps2sp <- function(xlim, ylim, l.out=100, proj="+init=epsg:4326") {

  m <- maps::map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p <- rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL <- sp::CRS(proj)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  m <- maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  bb <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(list(p))),"bb")), proj4string = LL)
  
  return(gIntersection(m, bb))
}
