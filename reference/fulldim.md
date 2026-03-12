# Reconstructs full dimensionality of MAgPIE objects

Deprecated: Use [`getItems`](getItems.md) instead.

## Usage

``` r
fulldim(x, sep = ".")
```

## Arguments

- x:

  A MAgPIE-object

- sep:

  A character separating joined dimension names

## Value

A list containing in the first element the dim output and in the second
element the dimnames output of the reconstructed array.

## Details

If a MAgPIE object is created from a source with more than one data
dimension, these data dimensions are combined to a single dimension.
fulldim reconstructs the original dimensionality and reports it.

## See also

[`as.magpie`](magpie-class.md),[`unwrap`](unwrap.md),[`wrap`](wrap.md)

Other ObjectInfo: [`dimExists()`](dimExists.md),
[`hasCoords()`](hasCoords.md), [`hasSets()`](hasSets.md),
[`is.temporal()`](is.temporal.md), [`isYear()`](isYear.md),
[`ncells()`](ncells.md), [`ndim()`](ndim.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich
