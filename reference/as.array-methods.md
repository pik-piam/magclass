# \~~ Methods for Function as.array \~~

\~~ Methods for function `as.array` \~~

## Usage

``` r
# S4 method for class 'magpie'
as.array(x)
```

## Arguments

- x:

  object which should be converted to an array

## Methods

- list("signature(x = \\ANY\\)"):

  standard as.array-method

- list("signature(x = \\magpie\\)"):

  Conversion takes place just by removing MAgPIE-object specific
  elements

## See also

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`unwrap()`](unwrap.md),
[`wrap()`](wrap.md)
