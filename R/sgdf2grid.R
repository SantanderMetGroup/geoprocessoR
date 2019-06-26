#'@title Convert a SpatialGridDataFrame to a C4R grid
#'@description Convert a SpatialGridDataFrame (package \pkg{sp}) to a C4R grid
#'@param sgdf A SpatialGridDataFrame, as returned by function \code{gdalwarp} (package \pkg{gdalUtils}).
#'@param varName Character with the variable name for the resulting grid.
#'@param level Character with the level for the resulting grid.
#'@param dates Named list of the form: list(start = NULL, end = NULL), for the resulting grid.
#'@param metadata Named list of characters with furher metadata for the resulting grid.
#'@seealso \code{\link[transformeR]{clim2sgdf}}
#'@return A C4R grid, with the metadata specified by the input arguments.
#'@keywords internal
#'@author M. Iturbide
#'@export
#'@importFrom transformeR mat2Dto3Darray

sgdf2grid <- function(sgdf, varName = NULL, level = NULL, dates = list(start = NULL, end = NULL), metadata = NULL) {
  grid <- list("Variable" = list("varName" = varName, "level" = level))
  df <- t(sgdf@data)
  x <- seq(sgdf@grid@cellcentre.offset[1], by = sgdf@grid@cellsize[1], length.out = sgdf@grid@cells.dim[1])
  y <- seq(sgdf@grid@cellcentre.offset[2], by = sgdf@grid@cellsize[2], length.out = sgdf@grid@cells.dim[2])
  grid[["Data"]] <- mat2Dto3Darray(df, x, y)
  grid[["xyCoords"]] <- list("x" = x, "y" = y)
  attr(grid[["xyCoords"]], "projection") <-  sgdf@proj4string
  attr(grid[["xyCoords"]], "resX") <- sgdf@grid@cellsize[1]
  attr(grid[["xyCoords"]], "resY") <- sgdf@grid@cellsize[2]
  grid[["Dates"]] <- dates
  if (!is.null(metadata)) {
    for (i in 1:length(metadata)) {
      attr(grid, names(metadata)[i]) <-  metadata[[i]]
    }
  }
  return(grid)
}

