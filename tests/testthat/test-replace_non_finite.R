context('Test replace_non_finite()')

test_that(
  desc = 'all instances of non-finite data are replaced', 
  code = {
    a <- new.magpie(letters[1:5], years = 'y1995', names = 'foo')
    b <- new.magpie(letters[1:5], years = 'y1995', names = 'foo', fill = 0)
    a[,,] <- c(0, NA, NaN, -Inf, Inf)
    
    expect_equal(object = replace_non_finite(a), b)
  }
)
