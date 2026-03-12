# dimCode

Function converts a dimension name or number to a dimension Code used
for MAgPIE objects

## Usage

``` r
dimCode(dim, x, missing = 0, strict = FALSE, sep = ".")
```

## Arguments

- dim:

  A vector of dimension numbers or dimension names which should be
  translated

- x:

  MAgPIE object in which the dimensions should be searched for.

- missing:

  Either a value to which a dimension should be set in case that it is
  not found (default is 0), or "stop" indicating that the function
  should throw an error in these cases.

- strict:

  if set to TRUE also properly set dimension names which refer to
  non-existing subdimensions will be treated as missing, otherwise these
  dimension codes will be returned, even if the subdimension does not
  exist

- sep:

  A character separating joined dimension names

## Value

A dimension code identifying the dimension. Either a integer which
represents the main dimensions (1=spatial, 2=temporal, 3=data) or a
numeric, representing the subdimensions of a dimension (e.g. 3.2 for the
second data dimension).

## See also

[`mselect`](mselect.md), [`getDim`](getDim.md)

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md), [`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimOrder()`](dimOrder.md),
[`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich, Kristine Karstens

## Examples

``` r
pop <- maxample("pop")
dimCode(c("t", "scenario", "blablub"), pop)
#>        t scenario  blablub 
#>        2        3        0 
```
