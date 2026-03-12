# as.SpatRasterDataset

Convert magclass object to a SpatRasterDataset object. Requires the
terra package.

## Usage

``` r
as.SpatRasterDataset(...)
```

## Arguments

- ...:

  arguments passed to as.SpatRaster

## Value

A SpatRasterDataset object

## Details

Calls [`as.SpatRaster`](as.SpatRaster.md) and then
[`spatRasterToDataset`](spatRasterToDataset.md).

## See also

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`unwrap()`](unwrap.md),
[`wrap()`](wrap.md)

## Author

Pascal Sauer
