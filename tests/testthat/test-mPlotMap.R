test_that("mPlotMap renders a single-panel coordinate-based object without error", {
  # Make sure no output is rendered
  withr::local_pdf(NULL)

  a <- maxample("animal")
  capture_output(expect_no_error(mPlotMap(a[1:10, 1, 1])))
})

test_that("mPlotMap auto-facets objects with multiple data names", {
  a <- maxample("animal")
  result <- mPlotMap(a[1:5, 1, 1:2], draw = FALSE)
  expect_s3_class(result, "ggplot")
  expect_s3_class(result$facet, "FacetWrap")
})

test_that("mPlotMap returns a ggplot object invisibly", {
  a <- maxample("animal")
  result <- mPlotMap(a[1:10, 1, 1], draw = FALSE)
  expect_s3_class(result, "ggplot")
})

test_that("mPlotMap errors on objects without coordinates", {
  expect_error(mPlotMap(maxample("pop")), "coordinates")
})

test_that("mPlotMap zooms to the extent of the data", {
  a <- maxample("animal")
  result <- mPlotMap(a[1:10, 1, 1], draw = FALSE)

  # The coordinate system should be limited to (a padded box around) the data,
  # not span the entire globe.
  coords <- getCoords(a[1:10, 1, 1])
  expect_true(all(is.finite(result$coordinates$limits$x)))
  expect_true(all(is.finite(result$coordinates$limits$y)))
  expect_lt(diff(result$coordinates$limits$x), 360)
  expect_gte(min(coords[[1]]), result$coordinates$limits$x[1])
  expect_lte(max(coords[[1]]), result$coordinates$limits$x[2])
})

test_that("mPlotMap warns when extra spatial subdimensions overlap cells", {
  a <- maxample("animal")[1:4, 1, 1]
  b <- a
  dimnames(b)[[1]] <- sub("([0-9]+)$", "9\\1", dimnames(b)[[1]])
  overlapping <- mbind(a, b)

  expect_warning(mPlotMap(overlapping, draw = FALSE), "more than one value per")
})

test_that("mPlotMap draws country outlines when maps is installed", {
  skip_if_not_installed("maps")

  a <- maxample("animal")[1:10, 1, 1]
  p <- mPlotMap(a, draw = FALSE)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% geoms)
})
