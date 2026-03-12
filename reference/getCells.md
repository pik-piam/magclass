# Get Cells

Extracts cell names of a MAgPIE-object

## Usage

``` r
getCells(x)

getCells(x) <- value

setCells(object, nm = "GLO")
```

## Arguments

- x, object:

  MAgPIE object

- value, nm:

  cell names the data should be set to.

## Value

getCells returns cell names of the MAgPIE-object, whereas setCells
returns the MAgPIE object with the manipulated cell names.

## Details

setCells is a shortcut to use a MAgPIE object with manipulated cell
names. setCells uses the variable names "object" and "nm" in order to be
consistent to the already existing function setNames.

## Functions

- `getCells(x) <- value`: set cell names

- `setCells()`: set cell names

## See also

[`getRegions`](getRegions.md), [`getNames`](getNames.md),
[`setNames`](setNames-methods.md), [`getCPR`](getCPR.md),
[`read.magpie`](read.magpie.md), [`write.magpie`](write.magpie.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getComment()`](getComment.md), [`getCoords()`](getCoords.md),
[`getDim()`](getDim.md), [`getItems()`](getItems.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- as.magpie(1)
 getCells(a)
#> [1] "GLO"
 setCells(a, "AFR")
#> AFR 
#>   1 
```
