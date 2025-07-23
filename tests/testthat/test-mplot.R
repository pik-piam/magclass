test_that("maxample runs without error for all combinations of global and total", {
  # Make sure no output is rendered
  # Open a null graphics device (e.g., PDF without a file)
  withr::local_pdf(NULL)

  for (example in c("pop", "animal", "bilateral")) {
    for (global in c(TRUE, FALSE)) {
      for (total in c(TRUE, FALSE)) {
        capture_output(expect_no_error(mplot(maxample(!!example), global = !!global, total = !!total)))
      }
    }
  }
})
