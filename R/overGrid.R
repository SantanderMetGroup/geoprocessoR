##     overGrid.R Grid scaling
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program. If not, see <http://www.gnu.org/licenses/>.


#' @title Spatial overlay for grids and Spatial* objects
#' @description Application of function \code{over} from \pkg{sp} to grids or
#' station data.
#' @param grid Input grid or station data. 
#' @param layer SpatialPolygons object from which the geometries or attributes are 
#' queried (type \code{help(package = "sp")}.
#' @param subset Logical (default is FALSE). If TRUE, spatial subsetting is performed over
#' the imput grid. Otherwise, grid-boxes outside \code{layer} are assigned as NA.
#' 
#' @details All grid locations outside layer are filled with NAs. 
#' 
#' @return A grid or station data.
#' 
#' @importFrom sp over bbox SpatialPoints CRS
#' @import transformeR 
#' 
#' @return A grid
#' @author M. Iturbide
#' @family subsetting
#' @export

overGrid <- function(grid, layer, subset = FALSE) {
      # commented lines are for posible runtime dimension consideration,
      # in which case function bindGrid.runtime should be created
      # grid <- redim(grid, runtime = TRUE)
      if (subset) grid <- subsetGrid(grid, lonLim = bbox(layer)[1,], latLim = bbox(layer)[2,], outside = TRUE)
      loc <- "loc" %in% getDim(grid)
      grid <- redim(grid, loc = loc)
      # n.run <- getShape(grid)["runtime"]
      n.mem <- getShape(grid)["member"]
      if (loc) {
            coords <- grid$xyCoords[, 2:1]
      } else {
            coords <- expand.grid(getCoordinates(grid)$y, getCoordinates(grid)$x)
      }
      # grr <- lapply(1:n.run, function(k){
            # grid.r <- subsetGrid(grid, runtime = k)
            grm <- lapply(1:n.mem, function(x) {
                  grl <- redim(subsetGrid(grid, members = x), member = FALSE, loc = loc)
                  dimNames.sub <- getDim(grl)
                  if (loc) {
                        dat <- grl$Data
                  } else {
                        dat <- array3Dto2Dmat(grl$Data)
                  }
                  a <- sp::SpatialPointsDataFrame(cbind(coords[,2], coords[,1]), data.frame(t(dat)), 
                                                  proj4string = CRS(attr(grl$xyCoords, "projection")), match.ID = FALSE)
                  a[which(is.na(over(a, layer))),] <- NA 
                  if (subset & loc) a <- a[which(!is.na(over(a, layer))),]
                    
                  coords <- as.data.frame(coordinates(a))
                  colnames(coords) <- c("x", "y")
                  if (loc) {
                        grl$Data <- unname(as.matrix(t(a@data)))
                        grl$xyCoords <- coords
                  } else {
                        grl$Data <- mat2Dto3Darray(t(a@data), unique(coords$x), unique(coords$y))
                        grl$xyCoords$x <- unique(coords$x)
                        grl$xyCoords$y <- unique(coords$y)
                  }
                  attr(grl$Data, "dimensions") <- dimNames.sub
                  grl
            })
            if (n.mem > 1) {
                  newgrid <- do.call("bindGrid", c(grm, dimension = "member"))
            } else {
                  newgrid <- grm[[1]]
            }
      # })
        # newgrid <-  do.call("bindGrid.runtime", grr)
      return(newgrid)
}




