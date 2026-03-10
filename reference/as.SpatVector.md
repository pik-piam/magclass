# as.SpatVector

Convert magclass object to a SpatVector object. Requires the terra
package and requires the magclass object to provide the geometry of the
spatial entities as "geometry" attribute in "WKT" format. (see object
"m" in example).

## Usage

``` r
as.SpatVector(x)
```

## Arguments

- x:

  MAgPIE object

## Value

A SpatVector object

## See also

[`as.SpatRaster`](as.SpatRaster.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
   r <- terra::rast(ncols = 360, nrows = 180, nlyrs = 4)
   r[85:89, 176:179] <- (1:20 %*% t(1:4))
   r[15:19, 76:79] <-   (10 + 1:20 %*% t(1:4))
   names(r) <- c("y2000..bla", "y2001..bla", "y2000..blub", "y2001..blub")
   v <- terra::as.polygons(r)
   m <- as.magpie(v)
   attr(m, "geometry")
   attr(m, "crs")
   v2 <- as.SpatVector(m)
}
```
