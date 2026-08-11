test_that("maxample runs without error for all combinations of global and total", {
  # Make sure no output is rendered
  # Open a null graphics device (e.g., PDF without a file)
  withr::local_pdf(NULL)

  for (example in c("pop", "animal", "bilateral")) {
    px <- maxample(example)
    for (global in c(TRUE, FALSE)) {
      # without global, mplot renders one facet per spatial item, which dominates
      # runtime (144 facets for "bilateral"), so plot only a few of them
      x <- if (global) px else px[1:min(4, ncells(px)), , ]
      for (total in c(TRUE, FALSE)) {
        capture_output(expect_no_error(mplot(x, global = !!global, total = !!total)))
      }
    }
  }
})
