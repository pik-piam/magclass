# Wrap

Reshape an array or a matrix by permuting and/or joining dimensions.

## Usage

``` r
wrap(x, map = list(NA), sep = ".")
```

## Arguments

- x:

  An array

- map:

  A list of length equal to the number of dimensions in the reshaped
  array. Each element should be an integer vectors specifying the
  dimensions to be joined in corresponding new dimension. One element
  may equal NA to indicate that that dimension should be a join of all
  non-specified (remaining) dimensions. Default is to wrap everything
  into a vector.

- sep:

  A character separating joined dimension names

## Note

This function is extracted from the R.utils library which is licensed
under LGPL\>=2.1 and written by Henrik Bengtsson.

## See also

[`unwrap`](unwrap.md),[`fulldim`](fulldim.md)

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`unwrap()`](unwrap.md)

## Author

Henrik Bengtsson, Jan Philipp Dietrich
