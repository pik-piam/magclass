test_that("years is not converted if conversion to int fails", {
  df <- as.data.frame(new.magpie("AFR.1", "2000-02-02", "test", 123))
  expect_identical(as.character(df$Year), "2000-02-02")
  
  df <- as.data.frame(new.magpie("AFR.1", "some text", "test", 123))
  expect_identical(as.character(df$Year), "some text")

  df <- as.data.frame(new.magpie("AFR.1", "2000", "test", 123))
  expect_identical(as.character(df$Year), "2000")

  df <- as.data.frame(new.magpie("AFR.1", "y2000", "test", 123))
  expect_identical(as.character(df$Year), "2000")
  
  df <- as.data.frame(new.magpie("AFR.1","yy2000", "test", 123))
  expect_identical(as.character(df$Year), "yy2000")
  
})
