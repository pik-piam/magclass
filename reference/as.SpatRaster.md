# as.SpatRaster

Convert magclass object to a SpatRaster object. Requires the terra
package.

## Usage

``` r
as.SpatRaster(x, res = NULL)
```

## Arguments

- x:

  MAgPIE object

- res:

  spatial data resolution. If not provided it will be guessed.

## Value

A SpatRaster object

## See also

[`getCoords`](getCoords.md)

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`unwrap()`](unwrap.md),
[`wrap()`](wrap.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
   r <- terra::rast(ncols = 360, nrows = 180, nlyrs = 4)
   r[85:89, 176:179] <- (1:20 %*% t(1:4))
   r[15:19, 76:79] <-   (10 + 1:20 %*% t(1:4))
   names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
   m <- as.magpie(r)
   r2 <- as.SpatRaster(m)
}
```
