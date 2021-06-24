
a <- maxample("animal")

test_that("dimSums works", {
  ref1 <- new("magpie", 
              .Data = structure(c(0, 0, 1, 1, 6, 4, 7, 5, 6, 4, 7, 5, 0, 0, 0, 0),
                                .Dim = c(2L, 2L, 4L),
                                .Dimnames = list(x.y.country.cell = c("5p75.53p25.NLD.14084", "6p25.53p25.NLD.14113"), 
                                                 year.month.day = c("y2000.april.20", "y2000.may.20"),
                                                 type.color = c("animal.black", "animal.white", 
                                                                "animal.red", "animal.brown"))))
  expect_identical(dimSums(a[1:2,1:2,], dim = "species"), ref1)
  expect_identical(dimSums(a[1:2,1:2,], dim = 3.2), ref1)
  
  ref2 <- new("magpie", 
              .Data = structure(c(9397, 7653, 3475, 3345, 1900),
                                .Dim = c(1L, 1L, 5L), 
                                .Dimnames = list(fake = "GLO", year = NULL, 
                                                 data.data1.data2 = c("animal.rabbit.black", "animal.rabbit.white", 
                                                                      "animal.bird.black", "animal.bird.red",
                                                                      "animal.dog.brown"))))
  expect_identical(dimSums(a, dim = 1:2), ref2)
  expect_identical(as.array(dimSums(a, dim = 3))[,,], dimSums(as.array(a), dim = 3))
  expect_error(dimSums(1), "Input is neither an array nor a MAgPIE object")
  expect_error(dimSums(a, dim = 4), "Invalid dimension")
  
  ax <- a[1,1:2,1:4]
  getItems(ax, dim = 3, raw = TRUE) <- c("animal.rabbit.black", "animal.rabbit.white",
                                         "animal.bird.black", "animal.bird.white")  
  ref4 <- new("magpie", 
              .Data = structure(c(12, 15), .Dim = c(1L, 2L, 1L),
                                .Dimnames = list(fake = "5p75", 
                                                 year.year1.year2 = c("y2000.april.20", "y2000.may.20"),
                                                 data = "animal")))
  expect_identical(dimSums(ax, dim=c(3.2,3.3)), ref4)
  
  ref5 <- new("magpie",
              .Data = structure(c(0, 1, 13, 13), .Dim = c(1L, 1L, 4L),
                                .Dimnames = list(fake = "5p75", year = NULL,
                                                 data.data1.data2 = c("animal.rabbit.black", "animal.bird.black",
                                                                      "animal.rabbit.white", "animal.bird.white"))))
  expect_identical(dimSums(ax, dim=1:2), ref5)
  
  skip("not yet working")
  ref3 <- new("magpie", .Data = structure(25770, .Dim = c(1L, 1L, 1L), 
                                          .Dimnames = list(fake = "GLO", year = NULL, data = NULL)))
  expect_identical(dimSums(a, dim = 1:3), ref3)
  expect_identical(dimSums(ax[1,1,], dim=1:2), ax[1,1,])
})
