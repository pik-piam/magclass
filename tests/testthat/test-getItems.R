context("Item Manipulation Test")

test_that("getItems even works for objects without set names", {
  x <- new.magpie("GLO", 1995, "bla")
  names(dimnames(x)) <- NULL
  expect_identical(getItems(x, 1), "GLO")
  expect_identical(getItems(x, 2), "y1995")
  expect_identical(getItems(x, 3), "bla")
})

test_that("getItems works for missing dimnames", {
  x <- new.magpie("GLO", NULL, NULL)
  expect_identical(getItems(x, 3), NULL)
  expect_identical(getItems(x, 3.1), NULL)
  expect_identical(getItems(x, 3, split = TRUE)[[1]], NULL)
})

pop <- maxample("pop")

test_that("getItems replacement works in case of incorrect set names", {
  getItems(pop, dim = 1, raw = TRUE) <- paste0(getItems(pop, dim = 1), ".", 1:10)
  value <- paste0("x", 1:10)
  getItems(pop, dim = "x", maindim = 1) <- value
  expect_identical(getItems(pop, "x"), value)
})

test_that("getItems can add and replace (sub)dimensions and separators are replaced with commas", {
  x <- pop
  expect_silent(getItems(x, "j", maindim = 1) <- 1:dim(x)[1])
  expect_identical(dimnames(x)[[1]], paste0(dimnames(pop)[[1]], ".", 1:dim(x)[1]))
  expect_identical(names(dimnames(x))[1], "i.j")

  expect_silent(getItems(x, "i") <- paste0("A.", dim(x)[1]:1))
  expect_identical(dimnames(x)[[1]], paste0("Ap", dim(x)[1]:1, ".", 1:dim(x)[1]))
  expect_identical(names(dimnames(x))[1], "i.j")

  expect_silent(getItems(x, 1) <- getItems(pop, dim = 1, split = FALSE))
  expect_identical(getItems(x, 1), getItems(pop, 1))
  expect_false(grepl(".", names(dimnames(x))[[1]], fixed = TRUE))

  expect_silent(getItems(x, 1.2) <- rep("GLO", 10))
  expect_identical(unname(getSets(x)["d1.2"]), "newdim")
  expect_true(dimExists(1.2, x))
  expect_silent(getItems(x, 1.2) <- NULL)
  expect_false(dimExists(1.2, x))
  expect_warning(getItems(x, 1.2) <- NULL, "Nothing to do here")
  expect_error(getItems(pop, 1) <- NULL, "Cannot unset dimension")
  pop1 <- pop[1, , ]
  expect_silent(getItems(pop1, 1) <- NULL)
})

test_that("getItems returns errors for unsupported inputs", {
  x <- new.magpie(c("GLO", "GLA"), 1995, "bla")
  expect_error(getItems(x, 1:2) <- "blub", "not supported")
  expect_error(getItems(x, 1) <- "blub", "Wrong number of items")
  expect_error(getItems(x, 4) <- "blub", "main dimension is not specified")
  expect_error(getItems(x, 4, maindim = 4) <- "blub", "Unsupported maindim")

  expect_error(getItems(pop, "t", maindim = 1) <- 12, "dimension different to maindim")

  expect_error(getItems(pop, dim = 1.3), "Subdimension 1.3 does not exist")

  expect_silent(tmp <- getItems(pop, 1))
  names(tmp) <- tmp
  names(tmp)[1] <- "BLA"
  expect_error(getItems(pop, 1) <- tmp, "not all names match")
})

test_that("getItems maps entries when input vector is named", {
  x <- pop
  value <- 1:dim(x)[1]
  names(value) <- rev(getItems(x, 1))
  expect_silent(getItems(x, 1) <- value)
  expect_identical(getItems(x, 1), as.character(dim(x)[1]:1))

  # test for subdimensioin
  x <- pop
  expect_warning(getItems(x, "j", maindim = 1) <- value, "Names of input vector are being ignored")
  expect_silent(x2 <- setItems(x, "i", value))
  expect_silent(getItems(x, "i") <- value)
  expect_identical(getItems(x, "i"), as.character(dim(x)[1]:1))
  expect_identical(x, x2)

  a <- maxample("animal")
  expect_error(getItems(a, 3.2) <- c("hase", "vogel"), "Wrong number of items")
  expect_error(getItems(a, 3.2) <- c(rabbit = "hase", bird = "vogel", cat = "hund"),
               "Names of input vector do not match")

  expect_silent(getItems(a, 3.2) <- c("hase", "vogel", "hund"))
  expect_identical(getItems(a, dim = 3), c("animal.hase.black", "animal.hase.white", "animal.vogel.black",
                                           "animal.vogel.red", "animal.hund.brown"))
  expect_error(getItems(a, 3.2) <- c("hase", "vogel"), "Wrong number of items")
  expect_error(getItems(a, 3.2) <- c("hase", "vogel"), "Wrong number of items")

  a2 <- maxample("animal")
  expect_silent(getItems(a2, 3.2) <- c(bird = "vogel", dog = "hund", rabbit = "hase"))
  expect_identical(a, a2)
})
