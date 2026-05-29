# tests/testthat/test-overGrid.R

library(climate4R.datasets)

# CRS constants used in the tests 
CRS_WGS84 <- "+proj=longlat +datum=WGS84 +no_defs"

# Helper functions 
make_layer_sp <- function() {
  # Simple rectangular polygon: longitudes (-6, -3), latitudes (40, 42)
  pts <- rbind(
    c(-6, 40), c(-3, 40),
    c(-3, 42), c(-6, 42),
    c(-6, 40)
  )
  Sr <- sp::Polygons(list(sp::Polygon(pts)), ID = "poly1")
  Sp <- sp::SpatialPolygons(list(Sr), proj4string = sp::CRS(CRS_WGS84))
  sp::SpatialPolygonsDataFrame(Sp, data = data.frame(id = 1), match.ID = FALSE)
}

is_xy_df <- function(xy) {
  is.data.frame(xy) && all(c("x","y") %in% names(xy))
}
is_xy_list <- function(xy) {
  is.list(xy) && all(c("x","y") %in% names(xy))
}

test_that("Regular grid, subset = FALSE: outside cells become NA and shape is preserved", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- CRS_WGS84
  layer <- make_layer_sp()

  # Save original shape
  shape0 <- getShape(grid)
  xy0x <- grid$xyCoords$x
  xy0y <- grid$xyCoords$y

  out <- overGrid(grid, layer = layer, subset = FALSE)

  # Same spatial structure (list x/y) and same grid shape
  expect_true(is_xy_list(out$xyCoords))
  expect_equal(length(out$xyCoords$x), length(xy0x))
  expect_equal(length(out$xyCoords$y), length(xy0y))
  expect_equal(getShape(out)[c("lat","lon")], shape0[c("lat","lon")])

  # Outside polygon -> NA, inside polygon -> values remain
  vals <- as.vector(out$Data)
  expect_true(any(is.na(vals)))
  expect_true(any(!is.na(vals)))
})

test_that("Regular grid, subset = TRUE: x/y constrained to layer bbox", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- CRS_WGS84
  layer <- make_layer_sp()

  out <- overGrid(grid, layer = layer, subset = TRUE)

  # For regular grids, xyCoords should be a list
  expect_true(is_xy_list(out$xyCoords))

  # Coordinates are within the polygon bounding box
  xmin <- -6; xmax <- -3; ymin <- 40; ymax <- 42

  dx <- min(diff(grid$xyCoords$x))
  dy <- min(diff(grid$xyCoords$y))

  expect_gte(min(out$xyCoords$x), xmin - dx)
  expect_lte(max(out$xyCoords$x), xmax + dx)
  expect_gte(min(out$xyCoords$y), ymin - dy)
  expect_lte(max(out$xyCoords$y), ymax + dy)
})

test_that("Stations (loc=TRUE), subset = FALSE: outside to NA; subset = TRUE: outside removed", {
  data("VALUE_Iberia_pr")
  st <- VALUE_Iberia_pr
  attr(st$xyCoords, "projection") <- CRS_WGS84
  layer <- make_layer_sp()

  # subset = FALSE: same number of stations, outside become NA
  outF <- overGrid(st, layer = layer, subset = FALSE)
  expect_true(is_xy_df(outF$xyCoords))
  expect_equal(nrow(outF$xyCoords), nrow(st$xyCoords))
  valsF <- as.vector(outF$Data)
  expect_true(any(is.na(valsF)))
  expect_true(any(!is.na(valsF)))

  # subset = TRUE: stations outside the polygon are removed
  suppressWarnings({
    outT <- overGrid(st, layer = layer, subset = TRUE)
  })

  expect_true(is_xy_df(outT$xyCoords))
  expect_lt(nrow(outT$xyCoords), nrow(st$xyCoords))

  # All remaining coordinates should be inside the polygon bbox
  xmin <- -6; xmax <- -3; ymin <- 40; ymax <- 42
  expect_true(all(outT$xyCoords$x >= xmin - 1e-6 & outT$xyCoords$x <= xmax + 1e-6))
  expect_true(all(outT$xyCoords$y >= ymin - 1e-6 & outT$xyCoords$y <= ymax + 1e-6))
})

test_that("Multi-member path: both members processed and remain identical after overlay", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- CRS_WGS84
  layer <- make_layer_sp()

  # Create a 2-member grid by duplicating the same data
  mg <- bindGrid(grid, grid, dimension = "member")
  shape_before <- getShape(mg)
  expect_true(shape_before["member"] == 2)

  mout <- overGrid(mg, layer = layer, subset = FALSE)
  shape_after <- getShape(mout)
  expect_true(shape_after["member"] == 2)
  
  # Both members should have identical data after overlay
  m1 <- subsetGrid(mout, members = 1)$Data
  m2 <- subsetGrid(mout, members = 2)$Data
  expect_equal(m1, m2)

  # xyCoords remains list-based with same spatial extent
  expect_true(is_xy_list(mout$xyCoords))
  expect_equal(length(mout$xyCoords$x), length(grid$xyCoords$x))
  expect_equal(length(mout$xyCoords$y), length(grid$xyCoords$y))
})
