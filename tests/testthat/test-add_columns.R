a <- maxample("animal")
attr(a, "Metadata") <- NULL

test_that("add_columns works", {
  expect_error(add_columns(a, dim = "bla"), "does not exist")
  expect_error(add_columns(a, dim = 1:2), "must be a single \\(sub\\)dimension")
  expect_error(add_columns(a, dim = 3.1, fill = 1:10), "fill value must be of length 1")
  a2 <- add_columns(a, addnm = c("horse", "magpie"), dim = "species", fill = 42)
  items <- c("animal.rabbit.black", "animal.rabbit.white", "animal.bird.black",
             "animal.bird.red", "animal.dog.brown", "animal.horse.black",
             "animal.horse.white", "animal.horse.red", "animal.horse.brown",
             "animal.magpie.black", "animal.magpie.white", "animal.magpie.red",
             "animal.magpie.brown")
  expect_identical(getItems(a2, dim = 3), items)
  expect_true(all(a2[, , "horse"] == 42))
  expect_identical(add_columns(a, dim = 3.2, addnm = character(0)), a)
  expect_identical(add_columns(a, dim = 3.2, addnm = NULL), a)
})
