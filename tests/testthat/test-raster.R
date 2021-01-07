context("Raster conversion tests")

skip_if_not_installed("raster")

test_that("raster convertion does not alter data", {
  for(i in c(0.5,1,2)) {
    r <- raster::raster(res=i)
    r[85:90,175:180] <- 1:36
    m <- as.magpie(r)
    r2 <- as.RasterLayer(m)
    expect_identical(r,r2)
    m2 <- as.magpie(r2)
    expect_identical(m,m2)
  }
})


