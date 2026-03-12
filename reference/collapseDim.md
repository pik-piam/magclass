# Collapse dataset dimensions

This function will remove names in the data dimension which are the same
for each element (meaning that this data dimension contains exactly one
element) or, if forced, remove any other subdimension. It is a
generalized version of the function [`collapseNames`](collapseNames.md)

## Usage

``` r
collapseDim(x, dim = NULL, keepdim = NULL)
```

## Arguments

- x:

  MAgPIE object

- dim:

  Either NULL, dimension code or name of dimension or a vector of these.
  If set to NULL all single entry subdimensions will be removed as they
  are irrelevant to uniquely identfy a data element. If specified, only
  the specified subdimensions will be removed (See
  [`dimCode`](dimCode.md) for more details how to specify a
  subdimension). CAUTION: The function also allows to specify
  subdimensions which are otherwise needed to clearly identify an entry.
  By removing these subdimensions duplicates in the data will be created
  potentially causing problems in the further use of the data set. Be
  careful in removing subdimensions.

- keepdim:

  (only considered if `dim` is not specified) Can be used to converse
  single element subdimension which otherwise would get deleted. If
  `dim` is specified this setting will not have any effect.

## Value

The provided MAgPIE object with collapsed dimensions

## Note

This function has some similarities to [`dimReduce`](dimReduce.md), but
serves a different purpose. While `collapseDim` only removes dimensions
which contain only a single element or which it is specifically told to
remove, [`dimReduce`](dimReduce.md) looks whether the entries of a
multi-entry dimension are all the same and removes dimensions for which
this is the case. In some cases both will lead to the same result but in
many other cases the results will differ.

## See also

[`getItems`](getItems.md) `"`[`magpie`](magpie-class.md)`"`

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimOrder()`](dimOrder.md), [`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
x <- new.magpie(c("GLO.1", "GLO.2"), 2000, c("bla.a", "bla.b"))
collapseDim(x)
#>        data1
#> region1  a  b
#>       1 NA NA
#>       2 NA NA
collapseDim(x, keepdim = 1:2)
#>               data1
#> region.region1  a  b
#>          GLO.1 NA NA
#>          GLO.2 NA NA
collapseDim(x, dim = 1.1)
#>        data.data1
#> region1 bla.a bla.b
#>       1    NA    NA
#>       2    NA    NA
collapseDim(x, dim = 3.2)
#>               data
#> region.region1 bla bla
#>          GLO.1  NA  NA
#>          GLO.2  NA  NA
```
