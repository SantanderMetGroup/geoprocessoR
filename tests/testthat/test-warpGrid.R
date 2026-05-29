# tests/testthat/test-warpGrid.R

library(climate4R.datasets)

# CRS constants used in the tests 
CRS_WGS84 <- "EPSG:4326"
CRS_POLAR <- "EPSG:3995"

# Helper functions 
is_c4r_grid <- function(x) {
  is.list(x) &&
    !is.null(x$Data) &&
    !is.null(x$xyCoords) &&
    !is.null(attr(x$Data, "dimensions"))
}

test_that("Returns a valid C4R grid and preserves metadata", {
  data("EOBS_Iberia_pr")
  grid <- EOBS_Iberia_pr
  clim <- climatology(grid)

  out <- NULL
  testthat::capture_output({
    out <- warpGrid(clim,
                    original.CRS = CRS_WGS84,
                    new.CRS = CRS_POLAR)
  })

  expect_true(is_c4r_grid(out))

  # Metadata is preserved
  expect_equal(getVarNames(out), getVarNames(clim))
  expect_equal(getGridVerticalLevels(out), getGridVerticalLevels(clim))
  expect_equal(getSeason(out), getSeason(clim))
  expect_equal(getRefDates(out, "start"), getRefDates(clim, "start"))
  expect_equal(getRefDates(out, "end"),   getRefDates(clim, "end"))

  # time dimension unchanged
  expect_equal(unname(getShape(out)["time"]),
               unname(getShape(clim)["time"]))

  # Coordinates exist
  if (is.list(out$xyCoords)) {
    expect_true(length(out$xyCoords$x) > 0)
    expect_true(length(out$xyCoords$y) > 0)
  } else {
    expect_true(nrow(out$xyCoords) > 0)
  }

  # Not all values are NA
  vals <- as.vector(out$Data)
  expect_true(any(!is.na(vals)))
})

test_that("Explicit CRS strings produces output with expected structure", {
  data("EOBS_Iberia_pr")
  clim <- climatology(EOBS_Iberia_pr)

  out <- NULL
  testthat::capture_output({
    out <- warpGrid(clim,
                    original.CRS = CRS_WGS84,
                    new.CRS = CRS_POLAR)
  })

  expect_true(is_c4r_grid(out))

  proj_attr <- attr(out$xyCoords, "projection")
  expect_true(is.null(proj_attr) || is.character(proj_attr))

  # Coordinates exist
  if (is.list(out$xyCoords)) {
    expect_true(length(out$xyCoords$x) > 0)
    expect_true(length(out$xyCoords$y) > 0)
  } else {
    expect_true(nrow(out$xyCoords) > 0)
  }
})

test_that("Works with different interpolation methods", {
  data("EOBS_Iberia_pr")
  clim <- climatology(EOBS_Iberia_pr)

  out_bilin <- NULL
  testthat::capture_output({
    out_bilin <- warpGrid(clim,
                          original.CRS = CRS_WGS84,
                          new.CRS = CRS_POLAR,
                          int.method = "bilinear")
  })

  out_near <- NULL
  testthat::capture_output({
    out_near <- warpGrid(clim,
                        original.CRS = CRS_WGS84,
                        new.CRS = CRS_POLAR,
                        int.method = "near")
  })


  expect_true(is_c4r_grid(out_bilin))
  expect_true(is_c4r_grid(out_near))

  # Shapes should match
  expect_equal(
    getShape(out_bilin)[names(getShape(out_bilin))],
    getShape(out_near)[names(getShape(out_near))]
  )
})

test_that("Preserves member dimension and warps all members equally", {
  data("EOBS_Iberia_pr")
  clim <- climatology(EOBS_Iberia_pr)

  # create a simple two-member grid
  mg <- bindGrid(clim, clim, dimension = "member")
  expect_equal(unname(getShape(mg)["member"]), 2)

  mout <- NULL
  testthat::capture_output({
    mout <- warpGrid(mg,
                    original.CRS = CRS_WGS84,
                    new.CRS = CRS_POLAR)
  })

  expect_true(is_c4r_grid(mout))
  expect_equal(unname(getShape(mout)["member"]), 2)

  # the two members should be identical after warping
  m1 <- subsetGrid(mout, members = 1)$Data
  m2 <- subsetGrid(mout, members = 2)$Data
  expect_equal(m1, m2)
})