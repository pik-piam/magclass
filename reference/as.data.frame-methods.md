# \~~ Methods for Function as.data.frame \~~

\~~ Methods for function `as.data.frame` \~~

## Usage

``` r
# S4 method for class 'magpie'
as.data.frame(x, rev = 1, raw = FALSE)
```

## Arguments

- x:

  A MAgPIE-object

- rev:

  The revision of the algorithm that should be used for conversion.
  rev=1 creates columns with the predefined names Cell, Region, Year,
  Data1, Data2,... and Value, rev=2 uses the set names of the MAgPIE
  object for naming and adds an attribute "dimtype" to the data.frame
  which contains information about the types of the different columns
  (spatial, temporal, data or value), rev=3 is identical to rev=2 except
  that characters are not being converted to factors (stringsAsFactors =
  FALSE).

- raw:

  Logical to control whether years beginning with "y" should be
  converted to integers (without "y") and coordinates should be
  converted to numerics. If set to raw columns are returned as they are
  in the initial object.

## Methods

- list("signature(x = \\magpie\\)"):

  Conversion creates columns for Cell, Region, Year, Data1, Data2,...
  and Value

## See also

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as_tibble.magpie()`](as_tibble.magpie.md), [`unwrap()`](unwrap.md),
[`wrap()`](wrap.md)

## Examples

``` r
pop <- maxample("pop")
head(as.data.frame(pop))
#>   Cell Region Year Data1     Value
#> 1   NA    AFR 1995    A2  552.6664
#> 2   NA    CPA 1995    A2 1280.6350
#> 3   NA    EUR 1995    A2  554.4384
#> 4   NA    FSU 1995    A2  276.3431
#> 5   NA    LAM 1995    A2  451.9981
#> 6   NA    MEA 1995    A2  277.7437
head(as.data.frame(pop, rev = 2))
#>     i    t scenario    .value
#> 1 AFR 1995       A2  552.6664
#> 2 CPA 1995       A2 1280.6350
#> 3 EUR 1995       A2  554.4384
#> 4 FSU 1995       A2  276.3431
#> 5 LAM 1995       A2  451.9981
#> 6 MEA 1995       A2  277.7437

a <- maxample("animal")
head(as.data.frame(a, rev = 3))
#>      x     y country  cell year month day   type species color .value
#> 1 5.75 53.25     NLD 14084 2000 april  20 animal  rabbit black      0
#> 2 6.25 53.25     NLD 14113 2000 april  20 animal  rabbit black      0
#> 3 6.75 53.25     NLD 14141 2000 april  20 animal  rabbit black      8
#> 4 4.75 52.75     NLD 14040 2000 april  20 animal  rabbit black      3
#> 5 5.75 52.75     NLD 14083 2000 april  20 animal  rabbit black      4
#> 6 6.25 52.75     NLD 14112 2000 april  20 animal  rabbit black      3
head(as.data.frame(a, rev = 3, raw = TRUE))
#>      x     y country  cell  year month day   type species color .value
#> 1 5p75 53p25     NLD 14084 y2000 april  20 animal  rabbit black      0
#> 2 6p25 53p25     NLD 14113 y2000 april  20 animal  rabbit black      0
#> 3 6p75 53p25     NLD 14141 y2000 april  20 animal  rabbit black      8
#> 4 4p75 52p75     NLD 14040 y2000 april  20 animal  rabbit black      3
#> 5 5p75 52p75     NLD 14083 y2000 april  20 animal  rabbit black      4
#> 6 6p25 52p75     NLD 14112 y2000 april  20 animal  rabbit black      3
attr(as.data.frame(a, rev = 3), "dimtype")
#>  [1] ".spat1" ".spat2" ".spat3" ".spat4" ".temp1" ".temp2" ".temp3"
#>  [8] ".data1" ".data2" ".data3" ".value"
```
