# cbind method for MAgPIE objects

cbind method for MAgPIE-objects. Issues a warning that mbind should be
used if all objects are magpie objects and continues afterwards.

## Usage

``` r
# S3 method for class 'magpie'
cbind(..., deparse.level = 1)
```

## Arguments

- ...:

  see [cbind](https://rdrr.io/r/base/cbind.html)

- deparse.level:

  see [cbind](https://rdrr.io/r/base/cbind.html)

## Value

A matrix as if cbind was applied to the internal vector of the MAgPIE
objects

## See also

Other DataBinding: [`extend()`](extend.md),
[`magpie_expand()`](magpie_expand.md), [`magpiesort()`](magpiesort.md),
[`matchDim()`](matchDim.md), [`mbind()`](mbind.md)

## Author

Patrick Rein
