# Collapse dataset names

This function has been superseded by [`collapseDim`](collapseDim.md)
which is a more generalized version of this function. Please use this
one instead!

## Usage

``` r
collapseNames(x, collapsedim = NULL, preservedim = NULL)
```

## Arguments

- x:

  MAgPIE object

- collapsedim:

  If you want to remove the names of particular dimensions provide the
  dimensions here. Since the function only works in the third dimension,
  you have to count from there on (e.g. dim = 3.2 refers to collapsedim
  = 2). Alternatively, you can also specify the name of the dimension.
  Default: NULL. CAUTION with parameter collapsedim! You could also
  force him to remove dimnames, which are NOT the same for each element
  and so create duplicates in dimnames.

- preservedim:

  If you want to remove the name of particular dimensions except some,
  you can specify the dimension(s) to preserve here. See collapsedim for
  naming convention. Note that preservedim will be ignored in the case,
  of a specified collapsedim

## Value

The provided MAgPIE object with collapsed names

## Details

This function will remove names in the data dimension which are the same
for each element (meaning that this data dimension contains exactly one
element)

## See also

[`collapseDim`](collapseDim.md), [`getItems`](getItems.md),
`"`[`magpie`](magpie-class.md)`"`

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md), [`collapseDim()`](collapseDim.md),
[`dimCode()`](dimCode.md), [`dimOrder()`](dimOrder.md),
[`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich, David Klein, Xiaoxi Wang
