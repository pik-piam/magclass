
p <- maxample("pop")
attr(p, "Metadata") <- NULL
getSets(p)[1:2] <- c("d1", "d2")

test_that("dimReduce works", {
p2 <- add_dimension(p, nm = paste0("scen", 1:5))
expect_identical(dimReduce(p2), p)

p2[, , ] <- setYears(p2[, 2, ], NULL)

expect_identical(dimReduce(p2), setYears(p[, 2, ], NULL))

p2[, , ] <- setCells(p2[1, , ], "GLO")
expect_identical(dimReduce(p2), setYears(setCells(p[1, 2, ], "GLO"), NULL))


})
