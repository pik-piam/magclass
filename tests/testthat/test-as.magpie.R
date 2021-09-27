context("Conversion Test")

mag <- maxample("pop")

test_that("conversions do not affect content", {
  expect_identical(as.magpie(mag), mag)
  expect_equivalent(as.magpie(as.array(mag)), mag)
  expect_equivalent(as.magpie(as.data.frame(mag)), mag)
  expect_null(as.magpie(NULL))

  empty <- new("magpie", .Data = structure(logical(0), .Dim = c(0L, 1L, 1L),
                                           .Dimnames = list(region = NULL, year = NULL,
                                                            data = NULL)))
  expect_identical(as.magpie(data.frame()), empty)
  df <- data.frame(x = 1:3, y = 4:6, row.names = c("a", "b", "c"))
  expect_identical(as.magpie(df), as.magpie(as.matrix(df)))

  ref <- new("magpie", .Data = structure(TRUE, .Dim = c(1L, 1L, 1L),
                                         .Dimnames = structure(list("GLO", NULL, "1.a"),
                                                               .Names = c("fake", NA, NA))))
  expect_identical(as.magpie(data.frame(a = TRUE)), ref)
  skip_if_not_installed("quitte")
  expect_equivalent(as.magpie(quitte::as.quitte(mag)), mag)
})

test_that("special cases work", {
  a <- as.array(mag)
  m2 <- as.magpie(a, spatial = 0, temporal = 0)
  expect_equal(dim(m2), c(1, 1, 320))

  names(dimnames(a)) <- NULL
  attr(a, "sets") <- c("spatial", "temporal", "data")
  m2 <- as.magpie(a)
  expect_equal(unname(getSets(m2)), attr(a, "sets"))

  bla <- data.frame(from = c("bla", "blub"), to = c("ble", "blo"), stringsAsFactors = FALSE)
  bla2 <- data.frame(from = c("bla", "blub"), to = c("ble", "blo"), stringsAsFactors = TRUE)
  blaExpect <- new("magpie",
                   .Data = structure(c("ble", "blo"),
                                     .Dim = c(1L, 1L, 2L),
                                     .Dimnames = list(region = "GLO", year = NULL,
                                                      from = c("bla", "blub"))))
  expect_identical(as.magpie(bla), blaExpect)
  expect_identical(as.magpie(bla2), blaExpect)
})

test_that("underscores are preserved", {
  mag2 <- maxample("pop")
  getCells(mag2) <- paste(getCells(mag2), 1:ncells(mag2), sep = "_")
  arr <- as.array(mag2)
  mag3 <- as.magpie(arr, spatial = 1)
  expect_identical(getCells(mag3), getCells(mag2))
})

test_that("error detection works", {
  expect_warning(m <- as.magpie(array(1, c(2, 2), list(c("ABC", "DEF"), c("GHI", "JKL")))), "No clear mapping")

  ref <- new("magpie", .Data = structure(c(1, 1, 1, 1), .Dim = c(2L, 1L, 2L),
                                         .Dimnames = structure(list(c("ABC", "DEF"), NULL, c("GHI", "JKL")),
                                                               .Names = c("fake", NA, NA))))
  expect_identical(m, ref)

  skip_if_not_installed("quitte")
  qi <- quitte::as.quitte(mag)
  qi$unit <- NULL
  expect_warning(as.magpie(qi), "does not follow the full quitte class definition")

})
