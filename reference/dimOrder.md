# dimOrder

Changes the order of the sub-dimension in a magpie object similar to
unwrapping and applying the aperm command, but more efficient.

## Usage

``` r
dimOrder(x, perm, dim = 3)
```

## Arguments

- x:

  magpie object

- perm:

  vector with the new order of the sub-dimension. Missing sub-dimensions
  will added automatically at the end

- dim:

  main dimension in which the order of sub-dimensions should be changed
  (1, 2 or 3)

## Value

magpie object

## See also

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md), [`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich, Benjamin Leon Bodirsky

## Examples

``` r
a <- maxample("animal")
head(a)
#> , , type.species.color = animal.rabbit.black
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8           10            13
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             17           23            31
#> 
#> , , type.species.color = animal.rabbit.white
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              6            7             9
#>   6p25.53p25.NLD.14113              4            5             6
#>   6p75.53p25.NLD.14141              4            5             6
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084             11           14            18
#>   6p25.53p25.NLD.14113              7            9            11
#>   6p75.53p25.NLD.14141              7            9            11
#> 
head(dimOrder(a, perm = 3:1, dim = 1))
#> , , type.species.color = animal.rabbit.black
#> 
#>                       year.month.day
#> country.y.x.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   NLD.53p25.5p75.14084              0            0             0
#>   NLD.53p25.6p25.14113              0            0             0
#>   NLD.53p25.6p75.14141              8           10            13
#>                       year.month.day
#> country.y.x.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   NLD.53p25.5p75.14084              0            0             0
#>   NLD.53p25.6p25.14113              0            0             0
#>   NLD.53p25.6p75.14141             17           23            31
#> 
#> , , type.species.color = animal.rabbit.white
#> 
#>                       year.month.day
#> country.y.x.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   NLD.53p25.5p75.14084              6            7             9
#>   NLD.53p25.6p25.14113              4            5             6
#>   NLD.53p25.6p75.14141              4            5             6
#>                       year.month.day
#> country.y.x.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   NLD.53p25.5p75.14084             11           14            18
#>   NLD.53p25.6p25.14113              7            9            11
#>   NLD.53p25.6p75.14141              7            9            11
#> 
head(dimOrder(a, perm = c(2,1,3), dim = 3))
#> , , species.type.color = rabbit.animal.black
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8           10            13
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             17           23            31
#> 
#> , , species.type.color = rabbit.animal.white
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              6            7             9
#>   6p25.53p25.NLD.14113              4            5             6
#>   6p75.53p25.NLD.14141              4            5             6
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084             11           14            18
#>   6p25.53p25.NLD.14113              7            9            11
#>   6p75.53p25.NLD.14141              7            9            11
#> 
```
