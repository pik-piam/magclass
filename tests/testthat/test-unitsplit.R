test_that("unitsplit on vectors and strings works", {
  teststring <- "Emi|CO2|+|Energy (Mt CO2/yr)"
  expected <- data.frame(
    variable = "Emi|CO2|+|Energy",
    unit = "Mt CO2/yr"
  )
  expect_identical(unitsplit(teststring), expected)
  expect_identical(unitsplit(teststring, 1), expected)
  expect_identical(unitsplit(teststring, "variable"), expected)
  testvector <- c("Emi|CO2|+|Energy (Mt CO2/yr)", "Emi|CO|Land Use (Mt CO/yr)")
  expected <- data.frame(
    variable = c("Emi|CO2|+|Energy", "Emi|CO|Land Use"),
    unit = c("Mt CO2/yr", "Mt CO/yr")
  )
  expect_identical(unitsplit(testvector), expected)
  expect_identical(unitsplit(testvector, 1), expected)
  expect_identical(unitsplit(testvector, "variable"), expected)
})

test_that("simple unitsplit works", {
  df <- data.frame(
    Model = c("REMIND", "REMIND", "REMIND"),
    Scenario = c("everything nice", "everything awful", "middle of the road"),
    Region = c("GLO", "GLO", "GLO"),
    Data = c("floor covering|textile|carpet|red|length (m)",
             "floor covering|textile|carpet|red|length - for our american friends (inch)",
             "floor covering|textile|carpet|red|length (cm)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    Data = c("floor covering|textile|carpet|red|length",
             "floor covering|textile|carpet|red|length - for our american friends",
             "floor covering|textile|carpet|red|length"),
    unit = c("m", "inch", "cm"),
    Model = c("REMIND", "REMIND", "REMIND"),
    Scenario = c("everything nice", "everything awful", "middle of the road"),
    Region = c("GLO", "GLO", "GLO"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_identical(unitsplit(df, 4), expected)
  expect_identical(unitsplit(df, "Data"), expected)
})

test_that("unitsplit works with braces", {
  df <- data.frame(
    Model = c("REMIND", "REMIND", "REMIND"),
    Scenario = c("everything nice", "everything awful", "middle of the road"),
    Region = c("GLO", "GLO", "GLO"),
    Data = c("floor covering|textile|carpet|red|length (m)",
             "floor covering|textile|carpet|red|length (for our american friends) (inch)",
             "floor covering|textile|carpet|red|length (cm)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    Data = c("floor covering|textile|carpet|red|length",
             "floor covering|textile|carpet|red|length (for our american friends)",
             "floor covering|textile|carpet|red|length"),
    unit = c("m", "inch", "cm"),
    Model = c("REMIND", "REMIND", "REMIND"),
    Scenario = c("everything nice", "everything awful", "middle of the road"),
    Region = c("GLO", "GLO", "GLO"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_identical(unitsplit(df, 4), expected)
  expect_identical(unitsplit(df, "Data"), expected)
})

test_that("unitsplit handles all cases", {
  wrapper <- function(inputstr) {
    df <- data.frame(c(inputstr), stringsAsFactors = FALSE)
    splitted <- unitsplit(df, 1)
    return(c(splitted[[1]], splitted[[2]]))
  }
  expect_identical(wrapper("length"), c("length", "N/A"))
  expect_identical(wrapper("length (m)"), c("length", "m"))
  expect_identical(wrapper("carpet (the good one)|length"),
                   c("carpet (the good one)|length", "N/A"))
  expect_identical(wrapper("carpet (the good one)|length (US) (inch)"),
                   c("carpet (the good one)|length (US)", "inch"))
  expect_identical(wrapper("carpet (the good one)|make (as given) ()"),
                   c("carpet (the good one)|make (as given)", ""))
  expect_identical(wrapper("Price|Agriculture|Corn|Index (Index (2020 = 1))"),
                   c("Price|Agriculture|Corn|Index", "Index (2020 = 1)"))
  expect_identical(wrapper("Price|Agriculture (with cows)|Corn|Index (Index (2020 = 1))"),
                   c("Price|Agriculture (with cows)|Corn|Index", "Index (2020 = 1)"))
  expect_identical(wrapper("Price|Agriculture|Corn|Index (based on 2020) ((1 + 2) / (6 - 3))"),
                   c("Price|Agriculture|Corn|Index (based on 2020)", "(1 + 2) / (6 - 3)"))
  expect_identical(wrapper("Price|Agriculture|Corn|Index (based on 2020 (-;) ((1 + 2) / (6 - 3))"),
                   c("Price|Agriculture|Corn|Index (based on 2020 (-;)", "(1 + 2) / (6 - 3)"))
  expect_identical(wrapper("Price|Agriculture|Corn|Index (based on 2020 :-)) ((1 + 2) / (6 - 3))"),
                   c("Price|Agriculture|Corn|Index (based on 2020 :-))", "(1 + 2) / (6 - 3)"))
})
