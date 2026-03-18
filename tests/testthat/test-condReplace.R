test_that("complete_magpie works", {
  m <- new.magpie(c("A", "B"), 2000, c("scen1", "scen2"), fill = rep(100, 4))

  # Base cases
  m2 <- m
  m2["A", , "scen1"] <- 2000
  replacedM <- condReplace(m2, m2 > 1000, 1000)
  expect_true(all(replacedM["A", , "scen1"] == 1000))

  m2 <- m
  m2["A", , "scen1"] <- NA
  replacedM <- condReplace(m2, is.na, 1000)
  expect_true(all(replacedM["A", , "scen1"] == 1000))

  # Expanding a magpie condition
  replacedM <- condReplace(m, new.magpie("B", fill = TRUE), 2)
  expect_true(all(replacedM["B", , ] == 2))

  # Expanding a magpie replacement with existing values
  m2 <- m
  m2["A", , "scen1"] <- 2000
  m2["B", , "scen1"] <- 2000
  replacedM <- condReplace(m, m2 > 1000, new.magpie("A", "B", fill = 2))
  expect_true(all(replacedM[, , "scen1"] == 2))

  # Expanding a magpie replacement with missing values
  m2 <- m
  m2["A", , "scen1"] <- 2000
  m2["B", , "scen2"] <- 2000
  replacedM <- condReplace(m, m2 > 1000, new.magpie("A", fill = 2))
  expect_true(all(replacedM["A", , "scen1"] == 2))
  expect_true(all(replacedM["B", , "scen2"] == 2))
})