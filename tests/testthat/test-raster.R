context("Raster conversion tests")

skip("blub")
skip_if_not_installed("raster")

test_that("raster convertion does not alter data", {
  for(i in c(0.5,1,2)) {
    for(j in 1:2) {
      r <- raster::brick(ncols=360/i,nrows=180/i, nl=1)
      r[85:90,175:180] <- 1:36
      m <- as.magpie(r)
      r2 <- as.RasterBrick(m)
      expect_identical(r,r2)
      m2 <- as.magpie(r2)
      expect_identical(m,m2)
    }
  }
})


