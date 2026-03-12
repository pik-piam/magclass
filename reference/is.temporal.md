# is.temporal, is.spatial

Functions to find out whether a vector consists of strings consistent
with the definition for auto-detection of temporal or spatial data.

## Usage

``` r
is.temporal(x)
```

## Arguments

- x:

  A vector

## Value

Returns TRUE or FALSE

## See also

Other ObjectInfo: [`dimExists()`](dimExists.md),
[`fulldim()`](fulldim.md), [`hasCoords()`](hasCoords.md),
[`hasSets()`](hasSets.md), [`isYear()`](isYear.md),
[`ncells()`](ncells.md), [`ndim()`](ndim.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
is.temporal(1991:1993)
#> [1] TRUE
is.spatial(c("GLO", "AFR"))
#> [1] TRUE
```
