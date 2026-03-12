# Replace Non-Finite Data

Replaces all instances of non-finite data (`NA`, `NaN`, `Inf`, and
`-Inf`).

## Usage

``` r
replace_non_finite(x, replace = 0)
```

## Arguments

- x:

  A vector or [`magpie`](magclass-package.md) object.

- replace:

  A value to replace non-finite data with.

## Value

A vector or [`magpie`](magclass-package.md) object, same as `x`.

## See also

Other Utility: [`unitjoin()`](unitjoin.md),
[`unitsplit()`](unitsplit.md)

## Author

Michaja Pehl

## Examples

``` r
part  <- new.magpie(letters[1:3], years = "y1995", names = "foo")
total <- new.magpie(letters[1:3], years = "y1995", names = "foo")

part[, , ]  <- c(0, 1, 2)
#> NOTE ([<-): Dangerous replacement! As replacement value is not an MAgPIE object name checking is deactivated!
total[, , ] <- c(0, 10, 10)
#> NOTE ([<-): Dangerous replacement! As replacement value is not an MAgPIE object name checking is deactivated!

part / total
#>   a   b   c 
#> NaN 0.1 0.2 

replace_non_finite(part / total)
#>   a   b   c 
#> 0.0 0.1 0.2 
```
