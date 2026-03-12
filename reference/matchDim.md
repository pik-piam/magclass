# Match dimensions of a magpie object to those of a reference object

A helper that restricts and expands a magpie object `x` to the size of a
magpie object `ref`. Dimension names not present in `x` are added and
set to the value provided by `fill`. Dimension names not present in
`ref` are cropped.

## Usage

``` r
matchDim(x, ref, dim = 1:3, fill = NA)
```

## Arguments

- x:

  A `magpie` object to be modified.

- ref:

  A `magpie` object used as a reference for the modification. Returns
  `x` if `ref` is `NULL`.

- dim:

  Subset of dimensions for which the matching should be done. Can be
  either a number between 1 and 3 or a vector of these. Defaults to all
  dimensions (i.e. `1:3`).

- fill:

  Value to be set in new dimensions.

## Value

The modified `magpie` object.

## See also

Other DataBinding: [`cbind.magpie()`](cbind.magpie.md),
[`extend()`](extend.md), [`magpie_expand()`](magpie_expand.md),
[`magpiesort()`](magpiesort.md), [`mbind()`](mbind.md)

## Author

Falk Benke
