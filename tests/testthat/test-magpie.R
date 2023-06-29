library(testthat)

context("Subsetting test")

p <- maxample("pop")
a <- as.array(p)

mv <- getOption("magclass.verbosity")
on.exit(options(magclass.verbosity = mv))
options(magclass.verbosity = 2)

test_that("single element subsetting works", {
  expect_identical(p[11], a[11])
  expect_identical(p[3], a[3])
})

test_that("multi element subsetting works", {
  expect_equivalent(as.array(p[3, , ]),                a[3, , , drop = FALSE])
  expect_equivalent(as.array(p["FSU", , ]),            a[4, , , drop = FALSE])
  expect_equivalent(as.array(p[, 2005, ]),             a[, 2, , drop = FALSE])
  expect_equivalent(as.array(p[as.factor("PAS"), , "B1"]),     a[9, , 2, drop = FALSE])
  expect_equivalent(as.array(p["PAS", "y2005", "B1"]),         a[9, 2, 2, drop = FALSE])
  expect_equivalent(as.array(p[c("CPA", "CPA"), , ]),           a[c(2, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[list(c("CPA", "CPA")), , ]),     a[c(2, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[list(i = c("CPA", "CPA")), , ]),   a[c(2, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[c("EUR", "CPA"), , ]),           a[c(3, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[list(c("EUR", "CPA")), , ]),     a[c(3, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[list(i = c("EUR", "CPA")), , ]),   a[c(3, 2), , , drop = FALSE])
  expect_equivalent(as.array(p[character(0), character(0), character(0)]), a[NULL, NULL, NULL, drop = FALSE])
  expect_identical(p[, NULL, ], p)
})

test_that("subsetting via dim argument works", {
  expect_identical(p[, 1:3, ], p[1:3, dim = 2])
  expect_identical(p[, , "B1"], p["B1", dim = 3])
  expect_identical(p[c("FSU", "EUR"), , ], p[c("FSU", "EUR"), dim = 1])
  expect_error(p[1, 2, dim = 2], "Only single dimension selection allowed")
  expect_error(p[1, 2, 3, dim = 2], "Only single dimension selection allowed")
  expect_error(p[1, dim = 1.2], "Invalid dim selection")
  p3 <- p2 <- p
  expect_silent(p2[, , "A2"] <- 99)
  expect_silent(p3["A2", dim = 3] <- 99)
  expect_identical(p2, p3)
  expect_silent(p2[c("FSU", "EUR"), , ] <- 42)
  expect_silent(p3[c("FSU", "EUR"), dim = 1] <- 42)
  expect_identical(p2, p3)
  expect_silent(p2[, 2015, ] <- -99)
  expect_silent(p3[2015, dim = 2] <- -99)
  expect_identical(p2, p3)
  expect_error(p3[1, 2, dim = 2] <- 1, "Only single dimension selection allowed")
  expect_error(p3[1, 2, 3, dim = 2] <- 1, "Only single dimension selection allowed")
  expect_error(p[1, dim = 1.2] <- 1, "Invalid dim selection")

})

test_that("boolean subsetting works", {
  expect_identical(p[p > 1000], p[p[10:1, , ] > 1000])
  p2 <- p[, 1, 1]
  expect_identical(p[p2 > 1000, , ], p[as.vector(p2 > 1000), , ])
  expect_identical(p[p2[10:1, , ] > 1000, , ], p[as.vector(p2 > 1000), , ])
  p2 <- p[1, , 1]
  expect_identical(p[, p2 > 1000, ], p[, as.vector(p2 > 1000), ])
  expect_identical(p[, p2[, 16:1, ] > 1000, ], p[, as.vector(p2 > 1000), ])
  expect_identical(p[, p2[, c(16:10, 1:9), ] > 1000, ], p[, as.vector(p2 > 1000), ])
})

test_that("error detection works", {
  expect_error(p[, , , ], "argument")
  expect_error(p[, , , blub = 42], "unknown argument\\(s\\) supplied")
  expect_error(p[, , , blub = 42] <- 42, "unknown argument\\(s\\) supplied")
  expect_error(p[, , "A3"], "out of bounds")
  expect_error(p[, , list("A3")], "out of bounds")
  expect_error(p[, , list(scenario = "A3")], "out of bounds")
  expect_error(p[, , list(blub = "A2")], "subdimension does not exist")

  names(dimnames(p)) <- NULL
  expect_error(p[, , list(scenario = "A2")], "subdimension does not exist \\(missing set names\\)") # nolint

  dimnames(p)[[3]] <- NULL
  expect_error(p[, , "A2"], "Missing element names")
})


test_that("invert argument works", {
  expect_identical(p[-1, , ],    p["AFR", invert = TRUE])
  expect_identical(p[-1, , ],    p["AFR", , invert = TRUE])
  expect_identical(p[-1, , ],    p["AFR", , , invert = TRUE])
  expect_identical(p[-9, -4, ],  p["PAS", 2025, invert = TRUE])
  expect_identical(p[-9, -4, ],  p["PAS", 2025, , invert = TRUE])
  expect_identical(p[, -4, ],    p[, 2025, , invert = TRUE])
  expect_identical(p[-1:-3, , ], p[1:3, , , invert = TRUE])
})

test_that("drop works", {
  a <- maxample("animal")
  expect_identical(getItems(a[, , , drop = TRUE], dim = 3)[1], "rabbit.black")
})

test_that("pmatch argument works", {
  expect_identical(getItems(p[, list("y1"), , pmatch = TRUE], 2), "y1995")
  expect_identical(getItems(p[, list(as.factor("y1")), , pmatch = TRUE], 2), "y1995")
  expect_identical(getItems(p[, "y1", , pmatch = TRUE], 2), "y1995")
  expect_error(getItems(p[, "y1", , pmatch = "right"], 2), "out of bounds")
  expect_identical(getItems(p[, "y1", , pmatch = "left"], 2), "y1995")
  expect_error(getItems(p[, "05", , pmatch = "left"], 2), "out of bounds")
  expect_identical(getItems(p[, "05", , pmatch = "right"], 2), c("y2005", "y2105"))
})

test_that("multiple subdimensions work", {
  getItems(p, "j", maindim = 1) <- 1:10
  expect_identical(p["AFR", , ], p[1, , ])
  expect_identical(p[list(i = "CPA"), , ], p[2, , ])
  expect_silent(p[list(i = "AFR"), , ] <- 99)
  expect_equal(as.vector(p["AFR", 1, 1]), 99)
  expect_silent(p[list(i = "AFR"), , list(scenario = "A2")] <- 100)
  expect_equal(as.vector(p["AFR", 1, "A2"]), 100)
  expect_silent(p[list("AFR"), 2145, list("A2")] <- 101)
  expect_equal(as.vector(p["AFR", 16, "A2"]), 101)

  # testing that years are properly handled
  t <- c(1995, 2005)
  yt <- paste0("y", t)
  expect_identical(p[, list(yt), ], p[, t, ])
  expect_identical(p[, list(t), ], p[, yt, ])
  expect_identical(getYears(p[, list(t), , invert = TRUE]), setdiff(getYears(p), yt))
})

test_that("value assignment works", {
  a <- maxample("animal")
  expect_silent(a[, NULL, as.factor("rabbit")] <- as.magpie(99))
  expect_true(all(a[, , "rabbit"] == 99))
  expect_silent(a[as.factor("NLD"), as.factor(c("april", "june")), as.factor("rabbit")] <- 12)
  expect_true(all(a["NLD", "june", "rabbit"] == 12))
  b <- a
  expect_silent(b[, , ] <- 0)
  expect_true(all(b[, , ] == 0))
  expect_silent(b[, , ] <- as.magpie(99))
  expect_true(all(b[, , ] == 99))
  expect_message(b[1:2, 1, 1] <- 1:2, "Dangerous replacement")
  expect_error(b[1:2, 1:2, 1:2] <- 1:7, "Different replacement length!")

  expect_silent(b["NLD", c("april", "june"), list("rabbit", "black")] <- a["NLD", c("april", "june"),
                                                                           list("rabbit", "black")])
  expect_identical(b["NLD", c("april", "june"), list("rabbit", "black")], a["NLD", c("april", "june"),
                                                                            list("rabbit", "black")])
})


test_that("data.frame subsetting works", {
  a <- maxample("animal")
  df <- data.frame(getItems(a, 3, split = TRUE, full = TRUE), stringsAsFactors = FALSE)
  w <- c(1, 3, 4)
  expect_identical(getItems(a[df[w, ]], 3), getItems(a, 3)[w])
  expect_identical(getItems(a[df[3:1][w, ]], 3), getItems(a, 3)[w])
  expect_identical(getItems(a[df[3:2][w, ]], 3), getItems(a, 3)[w])

  # Unknown dimensions to be added in output!
  df$blub <- paste0("bl", seq_len(dim(df)[1]))
  expect_identical(getItems(a[df[w, ]], 3), paste(getItems(a, 3), df$blub, sep = ".")[w])

  df2 <- df
  df2$ble <- paste0("ble", seq_len(dim(df2)[1]))
  expect_identical(getItems(a[df2[w, ]], 3), paste(getItems(a, 3), df2$blub, df2$ble, sep = ".")[w])

  # subselections work
  df$species <- NULL
  expect_identical(getItems(a[df[1, ]], 3), c("animal.rabbit.black.bl1", "animal.bird.black.bl1"))
  expect_identical(getItems(a[df[w, ]], 3), c("animal.rabbit.black.bl1", "animal.bird.black.bl1",
    "animal.rabbit.black.bl3", "animal.bird.black.bl3",
    "animal.bird.red.bl4"))

  df2 <- df
  df2$type <- NULL
  expect_identical(getItems(a[df2[1, ]], 3), c("animal.rabbit.black.bl1", "animal.bird.black.bl1"))


  # rows in df but not in a will get added with value NA
  df[3, 1] <- "car"
  expect_message(b <- a[df[w, ]], "elements were added")
  expect_identical(getItems(b, 3), c("animal.rabbit.black.bl1", "animal.bird.black.bl1",
    "animal.bird.red.bl4", "car.NA.black.bl3"))
  expect_true(all(is.na(b[, , "car.NA.black.bl3"])))

  df[4, 1] <- "house"
  expect_message(b <- a[df[w, ]], "elements were added")
  expect_identical(getItems(b, 3), c("animal.rabbit.black.bl1", "animal.bird.black.bl1",
    "car.NA.black.bl3", "house.NA.red.bl4"))


  df1 <- data.frame(getItems(a, 1, split = TRUE, full = TRUE))
  expect_identical(getItems(a[df1[w, ]], 1), getItems(a, 1)[w])
  df2 <- data.frame(getItems(a, 2, split = TRUE, full = TRUE))
  expect_identical(getItems(a[df2[w, ][c(3, 1, 2)]], 2), getItems(a, 2)[w])

  names(df2)[2] <- names(df2)[1]
  expect_error(a[df2], "more than once")

  names(df2)[2] <- "country"
  expect_error(a[df2], "must only contain subdimensions with a shared main dimension")


  names(df2) <- paste0("bla", seq_along(df2))
  expect_error(a[df2], "None of the dimensions in the mapping could be found")
  names(dimnames(a)) <- NULL
  expect_error(a[df], "must have names")

  # check that it works for single subdimension
  p <- maxample("pop")
  df <- data.frame(getItems(p, 3, split = TRUE, full = TRUE), stringsAsFactors = FALSE)
  df$blub <- paste0("bla", seq_len(nrow(df)))
  expect_identical(getItems(p[df], 3), paste0(getItems(p, 3), ".", df$blub))

  # check that it works if not element can be found in object
  df$scenario <- c("C1", "D2")
  expect_identical(getItems(p[df], 3), c("C1.bla1", "D2.bla2"))
  expect_true(all(is.na(p[df])))

  # check that empty magclass objects can get expanded
  p0 <- p[, , NULL]
  expect_true(all(is.na(p0[df])))
  expect_identical(getItems(p0[df], 3), c("C1.bla1", "D2.bla2"))
})

test_that("duplicates detection works", {
  a <- maxample("animal")
  expect_warning(a[, c(1, 1, 2, 3), ][, "y2000.april.20", ], "contain duplicates")
})
