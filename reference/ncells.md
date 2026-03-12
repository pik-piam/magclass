# Count elements

Functions to count the number of cells/years/datasets/regions of an
MAgPIE-object

## Usage

``` r
ncells(x)

ndata(x)

nregions(x)

nyears(x)
```

## Arguments

- x:

  A MAgPIE-object

## Value

- value:

  The number of cells/years/datasets/regions of `x`

## Functions

- `ndata()`: count datasets

- `nregions()`: count regions

- `nyears()`: count years

## See also

Other ObjectInfo: [`dimExists()`](dimExists.md),
[`fulldim()`](fulldim.md), [`hasCoords()`](hasCoords.md),
[`hasSets()`](hasSets.md), [`is.temporal()`](is.temporal.md),
[`isYear()`](isYear.md), [`ndim()`](ndim.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- is.magpie(NULL)
ncells(a)
#> NULL
nyears(a)
#> NULL
ndata(a)
#> NULL
nregions(a)
#> NULL
```
