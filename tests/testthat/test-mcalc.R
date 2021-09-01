
a <- maxample("animal")

test_that("mcalc works", {
   a2 <- setItems(2 * a[, , "rabbit"], dim = 3.2, "rabbit2")
   expect_identical(mcalc(a, `rabbit2` ~ 2 * rabbit), a2)
   expect_true(all(mcalc(a, `GLO` ~ 0.5 * NLD - 1 / 2 * NLD) == 0))
   expect_error(mcalc(a, `BLUB` ~ 10 * NOTanElement), "no match")
   getItems(a, dim = 3.1) <- "NLD"
   expect_error(mcalc(a, `BLUB` ~ 10 * NLD), "multiple matches")
   expect_silent(mcalc(a, `dog2` ~ 2 * dog, append = TRUE))
   expect_identical(a[, , "dog2"], mcalc(a, `dog2` ~ 2 * dog))
})

test_that("calculations work", {
   p <- maxample("pop")
   p1 <- setItems(p[, , 1], dim = 3, NULL)
   p0 <- p1[, , -1]
   expect_identical(p0, p1 * p0)
   expect_identical(p0, p0 * p1)
})
