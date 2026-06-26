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
