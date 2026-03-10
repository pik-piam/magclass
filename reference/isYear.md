# isYear

Function to find out whether a vector consists of strings in the format
"yXXXX" or "XXXX" with X being a number

## Usage

``` r
isYear(x, with_y = TRUE)
```

## Arguments

- x:

  A vector

- with_y:

  indicates which dataformat years have to have (4-digit without y
  (e.g.1984) or 5digit including y (y1984))

## Value

Returns a vector of the length of x with TRUE and FALSE

## Author

Benjamin Bodirsky

## Examples

``` r
x <- c("1955", "y1853", "12a4")
isYear(x, with_y = TRUE)
#> [1] FALSE  TRUE FALSE
isYear(x, with_y = FALSE)
#> [1]  TRUE FALSE FALSE
```
