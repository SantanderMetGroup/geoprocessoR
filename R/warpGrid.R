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
  
  # *** Helper functions to handle CRS ***
  crs_from_input <- function(value, arg_name) {
    if (inherits(value, "crs")) return(value)
    tryCatch(sf::st_crs(value), error = function(e) stop("Non-valid ", arg_name, " argument"))
  }

  crs_to_gdal_string <- function(crs_obj, arg_name) {
    if (is.na(crs_obj)) stop("Non-valid ", arg_name, " argument")
    if (!is.null(crs_obj$input) && !is.na(crs_obj$input) && nzchar(crs_obj$input)) return(crs_obj$input)
    if (!is.null(crs_obj$wkt) && !is.na(crs_obj$wkt) && nzchar(crs_obj$wkt)) return(crs_obj$wkt)
    stop("Non-valid ", arg_name, " argument")
  }

  original_crs <- crs_from_input(original.CRS, "original.CRS")
  new_crs <- crs_from_input(new.CRS, "new.CRS")

  original_crs_txt <- crs_to_gdal_string(original_crs, "original.CRS")
  new_crs_txt <- crs_to_gdal_string(new_crs, "new.CRS")

  # *** Convert C4R grid to stars ***
  pattern <- transformeR::grid2sp(data)
  pattern_stars <- suppressWarnings(stars::st_as_stars(pattern))
  sf::st_crs(pattern_stars) <- original_crs
  band_names <- names(pattern_stars)

  # *** Warp each band/member separately with GDAL ***
  nodata_value <- -9999

  warped_list <- lapply(band_names, function(nm) {
    srcfile <- tempfile(fileext = ".tif")
    dstfile <- tempfile(fileext = ".tif")
    on.exit(unlink(c(srcfile, dstfile), force = TRUE), add = TRUE)

    # Write the current band/member to a temporary GeoTIFF file with the original CRS
    suppressWarnings(
      stars::write_stars(
        pattern_stars[nm],
        dsn = srcfile,
        driver = "GTiff",
        NA_value = nodata_value))

    # Warp the GeoTIFF file to the new CRS using GDAL via sf::gdal_utils
    sf::gdal_utils(
      util = "warp",
      source = srcfile,
      destination = dstfile,
      options = c(
        "-s_srs", original_crs_txt,
        "-t_srs", new_crs_txt,
        "-r", int.method,
        "-dstnodata", as.character(nodata_value)),
      quiet = TRUE)

    suppressWarnings(stars::read_stars(dstfile, proxy = FALSE))
  })

  # *** Convert warped bands back to Spatial and recombine members ***
  warped_sp <- methods::as(warped_list[[1]], "Spatial")

  # Normalize nodata in first band
  if (!is.null(warped_sp@data) && ncol(warped_sp@data) >= 1) {
    warped_sp@data[[1]][warped_sp@data[[1]] == nodata_value] <- NA_real_
  }

  if (length(warped_list) > 1) {
    extra_cols <- lapply(warped_list[-1], function(w) {
      sp_tmp <- methods::as(w, "Spatial")
      v <- sp_tmp@data[[1]]
      v[v == nodata_value] <- NA_real_
      v
    })
    warped_sp@data <- data.frame(warped_sp@data, do.call(cbind, extra_cols))
  }
  names(warped_sp@data) <- band_names 

  # *** Back to C4R grid ***
  start <- getRefDates(data, which = "start")
  end <- getRefDates(data, which = "end")

  grid <- transformeR::sgdf2clim(
    sp = warped_sp,
    varName = getVarNames(data),
    level = getGridVerticalLevels(data),
    dates = list(start = start, end = end),
    season = getSeason(data))

  return(grid)
} 