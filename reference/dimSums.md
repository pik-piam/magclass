# Summation over dimensions

This function sums over any (sub-)dimension of a magpie object

## Usage

``` r
dimSums(x, dim = 3, na.rm = FALSE)
```

## Arguments

- x:

  A MAgPIE-object

- dim:

  The dimensions(s) to sum over. A vector of dimension codes or
  dimension names. See [`dimCode`](dimCode.md) for more information

- na.rm:

  logical. Should missing values (including NaN) be omitted from the
  calculations?

## Value

A MAgPIE object with values summed over the specified dimensions

## See also

[`rowSums`](https://rdrr.io/r/base/colSums.html),
[`getItems`](getItems.md), [`dimCode`](dimCode.md)

Other Aggregation: [`colSums-methods`](colSums-methods.md),
[`rowSums-methods`](rowSums-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- maxample("animal")
dimSums(a, dim = c(1, 2, 3.2))
#> animal.black animal.white   animal.red animal.brown 
#>        12872         7653         3345         1900 
dimSums(a, dim = c("x", "y", "cell", "month"))
#> , , type.species.color = animal.rabbit.black
#> 
#>        year.day
#> country y2000.20 y2001.20 y2002.20
#>     NLD      313      630     2823
#>     BEL      358      770     3798
#>     LUX       40       98      567
#> 
#> , , type.species.color = animal.rabbit.white
#> 
#>        year.day
#> country y2000.20 y2001.20 y2002.20
#>     NLD      364      758     3432
#>     BEL      286      547     2236
#>     LUX        9        9       12
#> 
#> , , type.species.color = animal.bird.black
#> 
#>        year.day
#> country y2000.20 y2001.20 y2002.20
#>     NLD      306      459      850
#>     BEL      336      489      890
#>     LUX       33       42       70
#> 
#> , , type.species.color = animal.bird.red
#> 
#>        year.day
#> country y2000.20 y2001.20 y2002.20
#>     NLD      339      492      894
#>     BEL      285      438      822
#>     LUX       12       21       42
#> 
#> , , type.species.color = animal.dog.brown
#> 
#>        year.day
#> country y2000.20 y2001.20 y2002.20
#>     NLD      255      255      340
#>     BEL      285      285      380
#>     LUX       30       30       40
#> 
```
