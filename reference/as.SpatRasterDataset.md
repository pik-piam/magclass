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

## Author

Pascal Sauer
