test_that("Turn magpie into tibble", {
  tib <- tibble::as_tibble(new.magpie("AFR.1", "2000", "test", 123))
  expect_s3_class(tib, "tbl_df")

  tib <- tibble::as_tibble(maxample("pop"))
  expect_s3_class(tib, "tbl_df")
})
