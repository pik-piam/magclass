# head/tail

head and tail methods for MAgPIE objects to extract the head or tail of
an object

## Usage

``` r
# S3 method for class 'magpie'
head(x, n1 = 3L, n2 = 6L, n3 = 2L, ...)
```

## Arguments

- x:

  MAgPIE object

- n1, n2, n3:

  number of lines in first, second and third dimension that should be
  returned. If the given number is higher than the length of the
  dimension all entries in this dimension will be returned.

- ...:

  arguments to be passed to or from other methods.

## Value

head returns the first n1 x n2 x n3 entries, tail returns the last n1 x
n2 x n3 entries.

## See also

[`head`](https://rdrr.io/r/utils/head.html),
[`tail`](https://rdrr.io/r/utils/head.html)

Other Display: [`maxample()`](maxample.md), [`mplot()`](mplot.md),
[`print.magpie()`](print.magpie.md), [`show-methods`](show-methods.md),
[`str.magpie()`](str.magpie.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
head(pop)
#> , , scenario = A2
#> 
#>      t
#> i         y1995   y2005   y2015   y2025   y2035   y2045
#>   AFR  552.6664  696.44  889.18 1124.11 1389.33 1659.73
#>   CPA 1280.6350 1429.53 1518.46 1592.09 1640.95 1671.94
#>   EUR  554.4384  582.36  593.76  605.27  614.58  618.97
#> 
#> , , scenario = B1
#> 
#>      t
#> i         y1995   y2005   y2015   y2025   y2035   y2045
#>   AFR  552.6664  721.85  932.04 1118.33 1267.33 1383.24
#>   CPA 1280.6350 1429.26 1499.74 1531.12 1518.73 1463.68
#>   EUR  554.4384  587.21  603.63  613.98  619.48  617.12
#> 
 tail(pop, 2, 4, 1)
#>      t
#> i       y2115   y2125   y2135   y2145
#>   PAS  507.06  507.06  507.06  507.06
#>   SAS 1528.15 1528.15 1528.15 1528.15
```
