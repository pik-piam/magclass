context("Lowpass Test")

p <- maxample("pop")

test_that("lowpass performs proper calculations", {
  # population data after 20 lowpass iterations for first two regions
  dat20 <- new("magpie", .Data = structure(c(959.55885559599, 1482.98153190583,
                                             1016.10215637913, 1498.22710355891, 1124.07386946257, 1526.47912463378,
                                             1274.05397151945, 1564.06542027954, 1453.69856012463, 1607.07162185219,
                                             1649.34145568655, 1652.33305222601, 1847.51308354974, 1697.82128947219,
                                             2036.2117975872, 1742.39786194564, 2205.87893781679, 1785.24256306564,
                                             2350.05785703802, 1825.37093631359, 2465.68152576388, 1861.51245252909,
                                             2552.91053574485, 1892.35240763826, 2614.49349387643, 1916.92025740035,
                                             2654.7374190261, 1934.85056911315, 2678.291126338, 1946.35308978183,
                                             2688.9922783188, 1951.91585988556), .Dim = c(2L, 16L, 1L),
                                           .Dimnames = list(i = c("AFR", "CPA"),
                                                            t = c("y1995", "y2005", "y2015", "y2025", "y2035",
                                                                  "y2045", "y2055", "y2065", "y2075", "y2085",
                                                                  "y2095", "y2105", "y2115", "y2125", "y2135",
                                                                  "y2145"), scenario = "A2")))

  expect_equivalent(lowpass(p[1:2, , 1], i = 20), dat20)
  expect_identical(lowpass(p[1:2, 1:5, ]), lowpass(p[1:2, c(5, 3, 1, 2, 4), ]))
  expect_warning(lowpass(p, fix = "start"), "does modify the total sum")
  expect_error(lowpass(p, fix = "blablub"), "not available")
  expect_identical(lowpass(p, i = 0), p)
  expect_identical(lowpass(c(5, 3, 21, 8)), c(4.5, 8, 13.25, 11.25))
  expect_identical(lowpass(c(5, 3, 21, 8), altFilter = 1:2), c(4.5, 9, 13.25, 11.25))
  expect_identical(as.vector(lowpass(setYears(as.magpie(c(5, 3, 21, 8), temporal = 1), 1:4), altFilter = 1:2)),
                   lowpass(c(5, 3, 21, 8), altFilter = 1:2))
})
