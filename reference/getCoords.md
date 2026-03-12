# Get Coordinates

Extracts spatial coordinates of a MAgPIE-object

## Usage

``` r
getCoords(x, xlab = "x", ylab = "y")

getCoords(x, xlab = "x", ylab = "y") <- value
```

## Arguments

- x:

  MAgPIE object

- xlab:

  label of x-dimension

- ylab:

  label of y-dimension

- value:

  coordinates as two column data.frame the data should be set to (first
  column = x, second column = y).

## Value

coordinates of the MAgPIE-object

## Functions

- `getCoords(x, xlab = "x", ylab = "y") <- value`: set coordinates

## See also

[`as.RasterBrick`](as.RasterBrick.md), [`getItems`](getItems.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getDim()`](getDim.md), [`getItems()`](getItems.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- maxample("animal")
getCoords(a)
#>       x     y
#> 1  5.75 53.25
#> 2  6.25 53.25
#> 3  6.75 53.25
#> 4  4.75 52.75
#> 5  5.75 52.75
#> 6  6.25 52.75
#> 7  6.75 52.75
#> 8  4.75 52.25
#> 9  5.25 52.25
#> 10 5.75 52.25
#> 11 6.25 52.25
#> 12 6.75 52.25
#> 13 4.25 51.75
#> 14 4.75 51.75
#> 15 5.25 51.75
#> 16 5.75 51.75
#> 17 3.25 51.25
#> 18 3.75 51.25
#> 19 4.25 51.25
#> 20 4.75 51.25
#> 21 5.25 51.25
#> 22 5.75 51.25
#> 23 3.25 50.75
#> 24 3.75 50.75
#> 25 4.25 50.75
#> 26 4.75 50.75
#> 27 5.25 50.75
#> 28 5.75 50.75
#> 29 4.25 50.25
#> 30 4.75 50.25
#> 31 5.25 50.25
#> 32 5.75 50.25
#> 33 5.25 49.75
#> 34 5.75 49.75
#> 35 6.25 49.75
```
