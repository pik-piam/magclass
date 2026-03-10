# Reconstructs full dimensionality of MAgPIE objects

If a MAgPIE object is created from a source with more than one data
dimension, these data dimensions are combined to a single dimension.
fulldim reconstructs the original dimensionality and reports it.

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

## See also

[`as.magpie`](magpie-class.md),[`unwrap`](unwrap.md),[`wrap`](wrap.md)

## Author

Jan Philipp Dietrich
