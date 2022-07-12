test_that("years is not converted if conversion to int fails", {
  df <- as.data.frame(new.magpie("AFR.1", "2000-02-02", "test", 123))
  expect_identical(as.character(df$Year), "2000-02-02")

  df <- as.data.frame(new.magpie("AFR.1", "some text", "test", 123))
  expect_identical(as.character(df$Year), "some text")

  df <- as.data.frame(new.magpie("AFR.1", "2000", "test", 123))
  expect_identical(as.character(df$Year), "2000")

  df <- as.data.frame(new.magpie("AFR.1", "y2000", "test", 123))
  expect_identical(as.character(df$Year), "2000")

  df <- as.data.frame(new.magpie("AFR.1", "yy2000", "test", 123))
  expect_identical(as.character(df$Year), "yy2000")

})

test_that("data.frame conversion works", {
  a <- maxample("animal")
  getItems(a, "x")[2] <- "-6p25"
  expect_silent(a2 <- as.data.frame(a, rev = 2))
  expect_silent(a3 <- as.data.frame(a, rev = 3))
  expect_silent(a3r <- as.data.frame(a, rev = 3, raw = TRUE))

  ea2 <- structure(list(x = c(5.75, -6.25), y = c(53.25, 53.25),
                        country = structure(c(1L, 1L), .Label = c("NLD", "BEL", "LUX"), class = "factor"),
                        cell = c(14084L, 14113L), year = c(2000L, 2000L),
                        month = structure(c(1L, 1L), .Label = c("april", "may", "june", "august"), class = "factor"),
                        day = c(20L, 20L), type = structure(c(1L, 1L), .Label = "animal", class = "factor"),
                        species = structure(c(1L, 1L), .Label = c("rabbit", "bird", "dog"), class = "factor"),
                        color = structure(c(1L, 1L), .Label = c("black", "white", "red", "brown"), class = "factor"),
                        .value = c(0, 0)), dimtype = c(".spat1", ".spat2", ".spat3", ".spat4", ".temp1", ".temp2",
                                                       ".temp3", ".data1", ".data2", ".data3", ".value"),
                   row.names = 1:2, class = "data.frame")

  ea3 <- structure(list(x = c(5.75, -6.25), y = c(53.25, 53.25), country = c("NLD", "NLD"),
                        cell = c(14084L, 14113L), year = c(2000L, 2000L),
                        month = c("april", "april"), day = c(20L, 20L),
                        type = c("animal", "animal"), species = c("rabbit", "rabbit"),
                        color = c("black", "black"), .value = c(0, 0)),
                   dimtype = c(".spat1", ".spat2", ".spat3", ".spat4", ".temp1", ".temp2",
                               ".temp3", ".data1", ".data2", ".data3", ".value"),
                   row.names = 1:2, class = "data.frame")

  ea3r <- structure(list(x = c("5p75", "-6p25"), y = c("53p25", "53p25"), country = c("NLD", "NLD"),
                         cell = c(14084L, 14113L), year = c("y2000", "y2000"), month = c("april", "april"),
                         day = c(20L, 20L), type = c("animal", "animal"), species = c("rabbit", "rabbit"),
                         color = c("black", "black"), .value = c(0, 0)),
                    dimtype = c(".spat1", ".spat2", ".spat3", ".spat4", ".temp1", ".temp2",
                                ".temp3", ".data1", ".data2", ".data3", ".value"),
                    row.names = 1:2, class = "data.frame")

  expect_identical(head(a2, n = 2), ea2)
  expect_identical(head(a3, n = 2), ea3)
  expect_identical(head(a3r, n = 2), ea3r)

  # test removal of empty dimensions
  b <- a[, 1, 1]
  getItems(b, dim = 2) <- NULL

  eb3 <- structure(list(x = c(5.75, -6.25, 6.75, 4.75, 5.75, -6.25), y = c(53.25, 53.25, 53.25, 52.75, 52.75, 52.75),
                        country = c("NLD", "NLD", "NLD", "NLD", "NLD", "NLD"),
                        cell = c(14084L, 14113L, 14141L, 14040L, 14083L, 14112L),
                        type = c("animal", "animal", "animal", "animal", "animal", "animal"),
                        species = c("rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit"),
                        color = c("black", "black", "black", "black", "black", "black"),
                        .value = c(0, 0, 8, 3, 4, 3)),
                   dimtype = c(".spat1", ".spat2", ".spat3", ".spat4", ".data1", ".data2", ".data3", ".value"),
                   row.names = c(NA, 6L), class = "data.frame")

  expect_silent(b3 <- as.data.frame(b, rev = 3))
  expect_identical(head(b3), eb3)

})
