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
#' @param original.CRS character or object as passed to function \code{\link[sf]{st_crs}} with the original projection.
#' Default to longlat projection (\code{"+init=epsg:4326"}).
#' @param new.CRS character string or object, as passed to function \code{\link[sf]{st_crs}}, specifying the target projection.
#' Default to polar stereographic projection (\code{"+init=epsg:3995"}).
#' @param int.method Resampling method. Default to \code{"bilinear"}. See details.
#' 
#' @return Warped grid with the structure of a C4R grid.
#' 
#' @details 
#' This function is a wrapper of the GDAL warping capabilities via \code{sf::gdal_utils("warp")}.
#' 
#'  \strong{int.method}
#'  
#'  By default bilinear interpolation is applied to get a complete grid in the target projection. Other options are \code{"near"}, \code{"cubic"},
#'   \code{"cubicspline"} etc., passed to GDAL through the \code{-r} option.

#' @export
#' @importFrom sf st_crs gdal_utils
#' @importFrom stars st_as_stars read_stars write_stars
#' @import transformeR
#' @author A. Casanueva, J. Bedia, M. Iturbide
#' @examples
#' library(climate4R.datasets)
#' data(ncep_hgt500_2000)
#' grid <- warpGrid(climatology(ncep_hgt500_2000))
#' # Example of application: plot in polar stereographic projection
#' library(visualizeR)
#' l1 <- get(load(paste0(find.package("visualizeR"), "/countries.rda"))) # world coastline
#' l1 <- sf::st_transform(sf::st_as_sf(l1[[2]]), crs = attr(grid$xyCoords, "projection"))
#' visualizeR::spatialPlot(grid, sp.layout = list(list(l1, first = FALSE)))

warpGrid <- function(data,
                     original.CRS = "+init=epsg:4326",
                     new.CRS = "+init=epsg:3995", 
                     int.method = "bilinear") {

  crs_to_gdal <- function(x, arg) {
    x <- if (inherits(x, "crs")) x else tryCatch(sf::st_crs(x), error = function(e) stop("Non-valid ", arg, " argument"))
    if (is.na(x)) stop("Non-valid ", arg, " argument")
    if (!is.null(x$input) && !is.na(x$input) && nzchar(x$input)) return(x$input)
    if (!is.null(x$wkt) && !is.na(x$wkt) && nzchar(x$wkt)) return(x$wkt)
    stop("Non-valid ", arg, " argument")
  }

  original.CRS <- crs_to_gdal(original.CRS, "original.CRS")
  new.CRS <- crs_to_gdal(new.CRS, "new.CRS")
  nodata <- -9999

  # *** CONVERT GRID TO A SpatialPointsDataFrame ***
  pattern <- transformeR::grid2sp(data)
  pattern <- suppressWarnings(stars::st_as_stars(pattern))
  sf::st_crs(pattern) <- sf::st_crs(original.CRS)
  band.names <- names(pattern)

  # *** WRITE A GDAL GRID MAP AND IMAGE RE-PROJECTION ***
  if (length(band.names) == 1) { # Case: single band, can be done in one step
    outf <- tempfile(fileext = ".tif")
    newf <- tempfile(fileext = ".tif")
    on.exit(unlink(c(outf, newf), force = TRUE), add = TRUE)

    # First step: write the grid to a GeoTIFF file, with the specified nodata value
    suppressWarnings(stars::write_stars(pattern, dsn = outf, driver = "GTiff", NA_value = nodata))

    # Second step: warp the GeoTIFF file to the new projection, using the specified interpolation method and nodata value
    sf::gdal_utils(util = "warp", source = outf, destination = newf, options = c("-s_srs", original.CRS, "-t_srs", new.CRS, "-r", int.method, "-dstnodata", as.character(nodata)), quiet = TRUE)

    # Third step: read the warped GeoTIFF file and convert it to a Spatial object, replacing nodata values with NA
    n <- suppressWarnings(stars::read_stars(newf, proxy = FALSE))
    n <- methods::as(n, "Spatial")
    n@data[[1]][n@data[[1]] == nodata] <- NA_real_
    names(n@data) <- band.names
  } else { # Case: multiple bands, need to be done separately and then merged
    # First step: write each band to a GeoTIFF file, with the specified nodata value
    outf <- vapply(band.names, function(z) {
      f <- tempfile(fileext = ".tif")
      suppressWarnings(stars::write_stars(pattern[z], dsn = f, driver = "GTiff", NA_value = nodata))
      f
    }, character(1))

    # Second step: build the VRT file that references the band files
    vrtf <- tempfile(fileext = ".vrt")
    newf <- tempfile(fileext = ".tif")
    on.exit(unlink(c(outf, vrtf, newf), force = TRUE), add = TRUE)
    sf::gdal_utils(util = "buildvrt", source = outf, destination = vrtf, options = c("-separate"), quiet = TRUE)

    # Third step: warp the VRT file to the new projection, using the specified interpolation method and nodata value
    sf::gdal_utils(util = "warp", source = vrtf, destination = newf, options = c("-s_srs", original.CRS, "-t_srs", new.CRS, "-r", int.method, "-dstnodata", as.character(nodata), "-wo", "UNIFIED_SRC_NODATA=NO"), quiet = TRUE)

    # Fourth step: extract each warped band to a separate GeoTIFF file
    band.files <- vapply(seq_along(band.names), function(i) {
      f <- tempfile(fileext = ".tif")
      sf::gdal_utils(util = "translate", source = newf, destination = f, options = c("-b", as.character(i)), quiet = TRUE)
      f
    }, character(1))
    on.exit(unlink(band.files, force = TRUE), add = TRUE)

    # Fifth step: read the separate band warped GeoTIFF files and merge them into a Spatial object, replacing nodata values with NA
    sp.list <- lapply(band.files, function(f) methods::as(suppressWarnings(stars::read_stars(f, proxy = FALSE)), "Spatial"))
    n <- sp.list[[1]]
    n@data <- as.data.frame(lapply(sp.list, function(x) {
      v <- x@data[[1]]
      v[v == nodata] <- NA_real_
      v
    }))
    names(n@data) <- band.names
  }

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