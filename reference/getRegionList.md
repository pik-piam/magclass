# Get a list of celluare region-belongings

Extracts a vector containing the region of each cell of a MAgPIE-object

## Usage

``` r
getRegionList(x)

getRegionList(x) <- value
```

## Arguments

- x:

  MAgPIE object

- value:

  A vector with ncell elements containing the regions of each cell.

## Value

A vector with ncell elements containing the region of each cell.

## Functions

- `getRegionList(x) <- value`: set region names

## See also

[`getRegions`](getRegions.md),[`getYears`](getYears.md),
[`getNames`](getNames.md), [`getCPR`](getCPR.md),
[`read.magpie`](read.magpie.md), [`write.magpie`](write.magpie.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getNames()`](getNames.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# a <- read.magpie("example.mz")
# getRegionList(a)
```
