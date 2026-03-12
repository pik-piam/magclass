# Get years

Extracts years of a MAgPIE-object

## Usage

``` r
getYears(x, as.integer = FALSE)

getYears(x) <- value

setYears(object, nm = NULL)
```

## Arguments

- x, object:

  MAgPIE object

- as.integer:

  Switch to decide, if output should be the used year-name (e.g.
  "y1995") or the year as integer value (e.g. 1995)

- value, nm:

  Years the data should be set to. Either supplied as a vector of
  integers or a vector of characters in the predefined year format
  ("y0000"). If only 1 year exist you can also set the name of the year
  to NULL.

## Value

getYears returns years of the MAgPIE-object, whereas setYears returns
the MAgPIE object with the manipulated years.

## Details

setYears is a shortcut to use a MAgPIE object with manipulated year
names. setYears uses the variable names "object" and "nm" in order to be
consistent to the already existing function setNames.

## Functions

- `getYears(x) <- value`: rename years

- `setYears()`: set years

## See also

[`getRegions`](getRegions.md), [`getNames`](getNames.md),
[`setNames`](setNames-methods.md), [`getCPR`](getCPR.md),
[`read.magpie`](read.magpie.md), [`write.magpie`](write.magpie.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getNames()`](getNames.md),
[`getRegionList()`](getRegionList.md), [`getRegions()`](getRegions.md),
[`getSets()`](getSets.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- as.magpie(1)
getYears(a)
#> NULL
 setYears(a, 1995)
#> [1] 1
```
