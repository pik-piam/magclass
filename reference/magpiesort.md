# MAgPIE-Sort

Brings the spatial and temporal structure of MAgPIE objects in the right
order. This function is especially useful when you create new MAgPIE
objects as the order typically should be correct for MAgPIE objects.

## Usage

``` r
magpiesort(x)
```

## Arguments

- x:

  MAgPIE object which might not be in the right order.

## Value

The eventually corrected MAgPIE object (right order in spatial in
temporal dimension)

## See also

`"`[`magpie`](magpie-class.md)`"`

Other DataBinding: [`cbind.magpie()`](cbind.magpie.md),
[`extend()`](extend.md), [`magpie_expand()`](magpie_expand.md),
[`matchDim()`](matchDim.md), [`mbind()`](mbind.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
a <- magpiesort(pop)
```
