# as.RasterBrick

Convert magclass object to a RasterBrick object

## Usage

``` r
as.RasterBrick(x, res = NULL)
```

## Arguments

- x:

  MAgPIE object

- res:

  spatial data resolution. If not provided it will be guessed.

## Value

A RasterBrick object

## See also

[`getCoords`](getCoords.md)

Other MAgPIE-Conversions: [`as.SpatRaster()`](as.SpatRaster.md),
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
if (FALSE) { # \dontrun{
if (requireNamespace("raster", quietly = TRUE)) {
   r <- raster::brick(ncols = 36, nrows = 18, nl = 4)
   r[14:18, 25:28] <- (1:20 %*% t(1:4))
   names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
   m <- as.magpie(r)
   r2 <- as.RasterBrick(m)
}
} # }
```
