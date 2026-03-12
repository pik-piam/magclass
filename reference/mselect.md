# MSelect

Select values from a MAgPIE-object

## Usage

``` r
mselect(x, ..., collapseNames = FALSE)

mselect(x, ...) <- value
```

## Arguments

- x:

  MAgPIE object

- ...:

  entry selections of the form `set_name=c(set_elem1,set_elem2)`.
  Alternatively a single list element containing these selections can be
  provided.

- collapseNames:

  Boolean which decides whether names should be collapsed or not.

- value:

  values on which the selected magpie entries should be set.

## Value

The reduced MAgPIE object containing only the selected entries or the
full MAgPIE object in which a selection of entries was manipulated.

## Details

This functions only work for MAgPIE objects with named dimensions as the
dimension name (set_name) has to be used to indicate in which dimension
the entries should be searched for!

## Functions

- `mselect(x, ...) <- value`: replace values in magpie object

## See also

[`collapseNames`](collapseNames.md), `"`[`magpie`](magpie-class.md)`"`

Other SelectionCalculation: [`magpply()`](magpply.md),
[`mcalc()`](mcalc.md), [`where()`](where.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
mselect(pop, i = c("AFR", "EUR"), scenario = "A2", t = "y2035")
#>     AFR     EUR 
#> 1389.33  614.58 
```
