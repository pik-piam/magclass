test_that("years is not converted if conversion to int fails", {
  df <- as.data.frame(new.magpie("AFR.1", "2000-02-02", "test", 123))
  expected <- data.frame(Cell = 1,
                         Region = "AFR",
                         Year = factor("2000-02-02"),
                         Data1 = factor("test"),
                         Value = 123)
  expect_equal(df, expected)

  df <- as.data.frame(new.magpie("AFR.1", "some text", "test", 123))
  expected <- data.frame(Cell = 1,
                         Region = "AFR",
                         Year = factor("some text"),
                         Data1 = factor("test"),
                         Value = 123)
  expect_equal(df, expected)

  df <- as.data.frame(new.magpie("AFR.1", "2000", "test", 123))
  expected <- data.frame(Cell = 1,
                         Region = "AFR",
                         Year = factor(2000),
                         Data1 = factor("test"),
                         Value = 123)
  expect_equal(df, expected)

  df <- as.data.frame(new.magpie("AFR.1", "y2000", "test", 123))
  expected <- data.frame(Cell = 1,
                         Region = "AFR",
                         Year = factor(2000),
                         Data1 = factor("test"),
                         Value = 123)
  expect_equal(df, expected)

  df <- as.data.frame(new.magpie("AFR.1","yy2000", "test", 123))
  expected <- data.frame(Cell = 1,
                         Region = "AFR",
                         Year = factor("yy2000"),
                         Data1 = factor("test"),
                         Value = 123)
  expect_equal(df, expected)
})
