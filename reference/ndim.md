# Count sub-dimensions

Functions to count the subdimensions of an MAgPIE-object

## Usage

``` r
ndim(x, dim = NULL)
```

## Arguments

- x:

  A MAgPIE-object

- dim:

  main dimension in which the sub-dimensions should be counted. If NULL
  the sum of all subdimensions is returned

## Value

Number of subdimensions

## See also

Other ObjectInfo: [`dimExists()`](dimExists.md),
[`fulldim()`](fulldim.md), [`hasCoords()`](hasCoords.md),
[`hasSets()`](hasSets.md), [`is.temporal()`](is.temporal.md),
[`isYear()`](isYear.md), [`ncells()`](ncells.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
  a <- maxample("animal")
  ndim(a)
#> [1] 10
  ndim(a,1)
#> [1] 4
  ndim(a,2)
#> [1] 3
  ndim(a,3)
#> [1] 3
```
