

p <- maxample("pop")
a <- maxample("animal")

test_that("convergence calculation works", {
 expect_error(convergence(1, 1), "no magpie object")
 expect_error(convergence(as.magpie(1), array(1)), "aim is no magpie object")
 expect_error(convergence(p, a), "regions have to be the same")
 expect_error(convergence(a, a[, , 1:3]), "dimnames have to be the same")
 expect_error(convergence(a, a[, 1:3, ]), "need the same timesteps")
 expect_error(convergence(p, p, 123345), "wrong year format")
 expect_error(convergence(p, p, direction = "unknown"), "Illegal direction")
 expect_error(convergence(p, p, type = "doesnotexist"), "does not exist")
 ref <- new("magpie", .Data = structure(c(553, 1281, 554, 349, 716, 293, 1, 2, 3),
                                        .Dim = c(3L, 3L, 1L),
                                        .Dimnames = list(i = c("AFR", "CPA", "EUR"),
                                                         t = c("y1995", "y2005", "y2015"), scenario = "A2")))
 p0 <- p[1:3, 1:3, 1]
 expect_identical(round(convergence(p0, 1:3, type = "linear")), ref)
 ref[, , ] <- c(553, 1281, 554, 345, 709, 290, 1, 2, 3)
 expect_identical(round(convergence(p0, 1:3, type = "s")), ref)
 ref[, , ] <- c(553, 1281, 554, 310, 636, 260, 82, 140, 57)
 expect_identical(round(convergence(p0, 1:3, type = "smooth")), ref)
 ref[, , ] <- c(553, 1281, 554, 262, 537, 220, 1, 2, 3)
 expect_identical(round(convergence(p0, 1:3, type = "decay")), ref)
 ref[, , ] <- c(553, 1281, 554, 848, 1430, 791, 1000, 1518, 1000)
 expect_identical(round(convergence(p0, 1000, direction = "up", type = "linear")), ref)
 ref[, , ] <- c(553, 1281, 554, 696, 1215, 582, 889, 1000, 594)
 p1000 <- p0[, 1, ]
 p1000[, , ] <- 1000
 expect_identical(round(convergence(p0, p1000, direction = "down", type = "linear")), ref)

 # test atypical region names in combination with numeric aim
 p2 <- p[, , 1]
 getItems(p2, dim = 1) <- paste0(getItems(p2, dim = 1), "XYZ")
 p20 <- p2
 p20[, , ] <- 0
 expect_identical(convergence(p2, 0), convergence(p2, p20))
})
