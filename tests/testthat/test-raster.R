context("Raster conversion tests")

skip_if_not_installed("raster")

test_that("raster convertion does not alter data", {
  expect_error(as.RasterBrick(1), "not a magpie object")
  for (i in c(0.5, 2)) {
    for (j in c(1, 4)) {
      for (t in c(1, 3)) {
        r <- raster::brick(ncols = 360 / i, nrows = 180 / i, nl = j * t)
        if (t > 1) {
          years <- paste0("y", 1900 + 1:t)
          data  <- paste0("bla", 1:j)
          names(r) <- paste0(rep(years, each = j), "..", rep(data, t))
        } else {
          names(r) <- paste0("bla", 1:j)
        }
        r[85:89, 176:179] <- (1:20 %*% t(1:(j * t)))
        m <- as.magpie(r)
        expect_equal(ndata(m), j)
        expect_equal(nyears(m), t)
        r2 <- as.RasterBrick(m)
        expect_identical(r[[names(r)]], r2[[names(r)]])
        m2 <- as.magpie(r2)
        expect_identical(m, m2)
      }
    }
  }
})
