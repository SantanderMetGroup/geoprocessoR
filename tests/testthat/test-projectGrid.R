# tests/testthat/test-projectGrid.R

library(climate4R.datasets)

# CRS constants used in the tests 
CRS_WGS84 <- "+proj=longlat +datum=WGS84 +no_defs"
CRS_RDNEW <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"

# Helper functions 
is_xy_df <- function(xy) {
  is.data.frame(xy) &&
    all(c("x","y") %in% names(xy)) &&
    is.numeric(xy$x) && is.numeric(xy$y)
}
is_xy_list <- function(xy) {
  is.list(xy) && all(c("x","y") %in% names(xy)) &&
    is.numeric(xy$x) && is.numeric(xy$y)
}

test_that("Throws error when original.CRS is missing and no projection info exists", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_error(
    projectGrid(grid, original.CRS = NA_character_, new.CRS = NA_character_),
    "Please define original.CRS"
  )
})

test_that("Throws error when original.CRS is invalid", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_error(
    projectGrid(grid, original.CRS = "+++this_is_not_a_valid_crs+++", new.CRS = NA_character_),
    "Non-valid original.CRS argument"
  )
})

test_that("Throws error when new.CRS is invalid", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_error(
    projectGrid(grid, original.CRS = CRS_WGS84, new.CRS = "+++not_valid_new_crs+++"),
    "Non-valid new.CRS argument"
  )
})

test_that("Redefines projection when orig.datum already exists and original.CRS is provided", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- "predefined"
  expect_warning(
    out <- projectGrid(grid, original.CRS = CRS_WGS84, new.CRS = NA_character_),
    "CAUTION! Grid with previusly defined projection:"
  )
  expect_match(attr(out$xyCoords, "projection"), "proj|epsg|WGS84", ignore.case = TRUE)
})

test_that("Uses predefined projection if original.CRS is NA, and fails if predefined is invalid", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  # Valid predefined CRS
  attr(grid$xyCoords, "projection") <- CRS_WGS84
  out_ok <- suppressMessages(  
    projectGrid(grid, original.CRS = NA_character_, new.CRS = NA_character_)
  )
  expect_true(is.character(attr(out_ok$xyCoords, "projection")))
  # Invalid predefined CRS string
  attr(grid$xyCoords, "projection") <- "this_is_not_a_valid_crs"
  expect_error(
    projectGrid(grid, original.CRS = NA_character_, new.CRS = NA_character_),
    "Grid with non-valid defined projection"
  )
})

test_that("Defines projection when orig.datum is NULL and original.CRS is valid", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_message(
    out <- projectGrid(grid, original.CRS = CRS_WGS84, new.CRS = NA_character_),
    "Arguments of the original projection defined as"
  )
  expect_true(!is.null(attr(out$xyCoords, "projection")))
  expect_true(is.character(attr(out$xyCoords, "projection")))
  expect_true(is.list(out$xyCoords) || is.data.frame(out$xyCoords))
})

test_that("Identity transformation on regular grid keeps it regular (list x/y + resX/resY > 0)", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_message(
    out <- projectGrid(grid, original.CRS = CRS_WGS84, new.CRS = CRS_WGS84),
    "Projecting.."
  )
  expect_true(is_xy_list(out$xyCoords))
  expect_true(!is.null(attr(out$xyCoords, "resX")))
  expect_true(!is.null(attr(out$xyCoords, "resY")))
  expect_true(is.numeric(attr(out$xyCoords, "resX")))
  expect_true(is.numeric(attr(out$xyCoords, "resY")))
  expect_true(attr(out$xyCoords, "resX") != 0)
  expect_true(attr(out$xyCoords, "resY") != 0)
  expect_match(attr(out$xyCoords, "projection"), "proj|epsg|WGS84", ignore.case = TRUE)
})

test_that("Real projection on regular grid becomes irregular (data.frame x,y + res=0)", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  attr(grid$xyCoords, "projection") <- NULL
  expect_message(
    out <- projectGrid(grid, original.CRS = CRS_WGS84, new.CRS = CRS_RDNEW),
    "Projecting.."
  )
  expect_true(is_xy_df(out$xyCoords))
  expect_identical(attr(out$xyCoords, "resX"), 0)
  expect_identical(attr(out$xyCoords, "resY"), 0)
  expect_match(attr(out$xyCoords, "projection"), "proj|epsg|utm|WGS84|zone", ignore.case = TRUE)
})

test_that("Single point is always treated as irregular (data.frame x,y + res=0)", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  lon <- grid$xyCoords$x[1]; lat <- grid$xyCoords$y[1]
  one <- subsetGrid(grid, lonLim = c(lon, lon), latLim = c(lat, lat), drop = FALSE)
  attr(one$xyCoords, "projection") <- NULL
  expect_message(
    out <- projectGrid(one, original.CRS = CRS_WGS84, new.CRS = CRS_RDNEW),
    "Projecting.."
  )
  expect_true(is_xy_df(out$xyCoords))
  expect_identical(attr(out$xyCoords, "resX"), 0)
  expect_identical(attr(out$xyCoords, "resY"), 0)
})

test_that("Station series (irregular from the start) project correctly", {
  data("VALUE_Iberia_pr")
  st <- VALUE_Iberia_pr
  attr(st$xyCoords, "projection") <- NULL
  expect_message(
    out <- projectGrid(st, original.CRS = CRS_WGS84, new.CRS = CRS_RDNEW),
    "Projecting.."
  )
  expect_true(is_xy_df(out$xyCoords))
  expect_identical(attr(out$xyCoords, "resX"), 0)
  expect_identical(attr(out$xyCoords, "resY"), 0)
})
