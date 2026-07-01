test_that("mPlotMap renders coordinate-based objects without error", {
  # Make sure no output is rendered
  withr::local_pdf(NULL)

  a <- maxample("animal")

  # single panel (one time slice, one data name)
  capture_output(expect_no_error(mPlotMap(a[, 1, 1])))

  # multiple data names -> auto-faceted
  capture_output(expect_no_error(mPlotMap(a[, 1, ])))
})

test_that("mPlotMap returns a ggplot object invisibly", {
  withr::local_pdf(NULL)

  a <- maxample("animal")
  p <- capture_output(result <- mPlotMap(a[, 1, 1]))
  expect_s3_class(result, "ggplot")
})

test_that("mPlotMap errors on objects without coordinates", {
  expect_error(mPlotMap(maxample("pop")), "coordinates")
})

test_that("mPlotMap zooms to the extent of the data", {
  withr::local_pdf(NULL)

  a <- maxample("animal")
  p <- capture_output(result <- mPlotMap(a[, 1, 1]))

  # The coordinate system should be limited to (a padded box around) the data,
  # not span the entire globe.
  coords <- getCoords(a[, 1, 1])
  expect_true(all(is.finite(result$coordinates$limits$x)))
  expect_true(all(is.finite(result$coordinates$limits$y)))
  expect_lt(diff(result$coordinates$limits$x), 360)
  expect_gte(min(coords[[1]]), result$coordinates$limits$x[1])
  expect_lte(max(coords[[1]]), result$coordinates$limits$x[2])
})
