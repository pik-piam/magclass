# Unwrap

Creates a higher dimensional array by separating all subdimensions in
the third dimension of a MAgPIE object and returning them as separate
dimension.

## Usage

``` r
unwrap(x, sep = NULL)
```

## Arguments

- x:

  A MAgPIE object

- sep:

  deprecated, please do not use anymore

## Value

An array with the full dimensionality of the original data

## See also

[`wrap`](wrap.md),[`fulldim`](fulldim.md)

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`wrap()`](wrap.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- as.magpie(array(1:6, c(3, 2), list(c("bla", "blub", "ble"), c("up", "down"))))
unwrap(a)
#> , , bla, up
#> 
#>     [,1]
#> GLO    1
#> 
#> , , blub, up
#> 
#>     [,1]
#> GLO    2
#> 
#> , , ble, up
#> 
#>     [,1]
#> GLO    3
#> 
#> , , bla, down
#> 
#>     [,1]
#> GLO    4
#> 
#> , , blub, down
#> 
#>     [,1]
#> GLO    5
#> 
#> , , ble, down
#> 
#>     [,1]
#> GLO    6
#> 
```
