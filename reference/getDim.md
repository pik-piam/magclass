# getDim

Function which tries to detect the dimension to which the given elems
belong

## Usage

``` r
getDim(elems, x, fullmatch = FALSE, dimCode = TRUE)
```

## Arguments

- elems:

  A vector of characters containing the elements that should be found in
  the MAgPIE object

- x:

  MAgPIE object in which elems should be searched for.

- fullmatch:

  If enabled, only dimensions which match exactly the elements provided
  will be returned. Otherwise, it is sufficient if elems contains a
  subset of the dimension.

- dimCode:

  If enabled, the dimCode will be returned, otherwise the name of the
  dimension.

## Value

The name or dimCode of the dimensions in which elems were found.

## See also

[`mcalc`](mcalc.md),[`dimCode`](dimCode.md)

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getItems()`](getItems.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
getDim(c("AFR", "CPA"), pop)
#> [1] 1.1
 getDim(c("AFR", "CPA"), pop, fullmatch = TRUE)
#> numeric(0)
 getDim(c("AFR", "CPA"), pop, dimCode = FALSE)
#> [1] "i"
```
