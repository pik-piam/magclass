# aperm method for magpie objects

Permutes the dimensions of a magpie object and returns a plain array.
Since the magpie class has fixed dimension semantics (spatial, temporal,
data), permuting the main dimensions produces an object that is no
longer a valid magpie. This method therefore always returns a plain
array.

## Usage

``` r
# S3 method for class 'magpie'
aperm(a, perm = NULL, ...)
```

## Arguments

- a:

  A magpie object.

- perm:

  An integer vector giving the new permutation of dimensions, or NULL
  for reverse order. See [`aperm`](https://rdrr.io/r/base/aperm.html).

- ...:

  Further parameters passed on to
  [`aperm`](https://rdrr.io/r/base/aperm.html).

## Value

A plain array (magpie class attribute dropped).

## Details

Use [`dimOrder`](dimOrder.md) to reorder sub-dimensions within a single
main dimension while preserving the magpie class.

## See also

[`dimOrder`](dimOrder.md), [`aperm`](https://rdrr.io/r/base/aperm.html)

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md), [`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimOrder()`](dimOrder.md), [`dimReduce()`](dimReduce.md)

## Author

Patrick Rein

## Examples

``` r
p <- maxample("pop")
a <- aperm(p, c(2, 1, 3))
class(a) # "array", not "magpie"
#> [1] "array"
```
