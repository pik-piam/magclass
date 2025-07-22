test_that("maxample runs without error for all combinations of global and total", {
  # Make sure no output is rendered
  # Open a null graphics device (e.g., PDF without a file)
  pdf(NULL)
  # Ensure the device is closed after the test
  on.exit(dev.off())

  for (example in c("pop", "animal", "bilateral")) {
    for (global in c(TRUE, FALSE)) {
      for (total in c(TRUE, FALSE)) {
        capture_output(expect_no_error(mplot(maxample(!!example), global = !!global, total = !!total)))
      }
    }
  }
})
