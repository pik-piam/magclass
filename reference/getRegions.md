# Get regions

Extracts regions of a MAgPIE-object

## Usage

``` r
getRegions(x)

getRegions(x) <- value
```

## Arguments

- x:

  MAgPIE object

- value:

  Vector containing the new region names of the MAgPIE objects. If you
  also want to change the mapping of regions to cell please use
  [`getRegionList`](getRegionList.md) instead.

## Value

Regions of the MAgPIE-object

## Functions

- `getRegions(x) <- value`: overwrite region names

## See also

[`getYears`](getYears.md), [`getNames`](getNames.md),
[`getCPR`](getCPR.md), [`read.magpie`](read.magpie.md),
[`write.magpie`](write.magpie.md), `"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getNames()`](getNames.md),
[`getRegionList()`](getRegionList.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# a <- read.magpie("example.mz")
# getRegions(a)
```
