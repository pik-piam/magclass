# Has Coordinates

Checks, whether object contains coordinates.

## Usage

``` r
hasCoords(x, xlab = "x", ylab = "y")
```

## Arguments

- x:

  MAgPIE object

- xlab:

  label of x-dimension

- ylab:

  label of y-dimension

## Value

Boolean indicating whether coordinates were found or not

## See also

[`getCoords`](getCoords.md)

Other ObjectInfo: [`dimExists()`](dimExists.md),
[`fulldim()`](fulldim.md), [`hasSets()`](hasSets.md),
[`is.temporal()`](is.temporal.md), [`isYear()`](isYear.md),
[`ncells()`](ncells.md), [`ndim()`](ndim.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
hasCoords(maxample("pop"))
#> [1] FALSE
hasCoords(maxample("animal"))
#> [1] TRUE
```
