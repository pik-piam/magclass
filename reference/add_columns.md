# add_columns

Function adds new columns to the existing magpie object.

## Usage

``` r
add_columns(x, addnm = "new", dim = 3.1, fill = NA)
```

## Arguments

- x:

  MAgPIE object which should be extended.

- addnm:

  The new elements that should be added to the (sub)dimension

- dim:

  The (sub)dimension to be filled either identified via name or
  dimension code (see [`dimCode`](dimCode.md) for more information)

- fill:

  fill value of length 1 for the newly added columns (NA by default)

## Value

The extended MAgPIE object

## See also

[`addDim`](addDim.md),[`dimCode`](dimCode.md)

Other DimensionManipulation: [`addDim()`](addDim.md),
[`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimOrder()`](dimOrder.md), [`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich, Benjamin Bodirsky

## Examples

``` r
a <- maxample("animal")
a2 <- add_columns(a, addnm = c("horse", "magpie"), dim = "species", fill = 42)
getItems(a2, dim = 3)
#>  [1] "animal.rabbit.black" "animal.rabbit.white" "animal.bird.black"  
#>  [4] "animal.bird.red"     "animal.dog.brown"    "animal.horse.black" 
#>  [7] "animal.horse.white"  "animal.horse.red"    "animal.horse.brown" 
#> [10] "animal.magpie.black" "animal.magpie.white" "animal.magpie.red"  
#> [13] "animal.magpie.brown"
getItems(a2, dim = 3, split = TRUE)
#> $type
#> [1] "animal"
#> 
#> $species
#> [1] "rabbit" "bird"   "dog"    "horse"  "magpie"
#> 
#> $color
#> [1] "black" "white" "red"   "brown"
#> 
head(a2[, , "magpie"])
#> , , type.species.color = animal.magpie.black
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084             42           42            42
#>   6p25.53p25.NLD.14113             42           42            42
#>   6p75.53p25.NLD.14141             42           42            42
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084             42           42            42
#>   6p25.53p25.NLD.14113             42           42            42
#>   6p75.53p25.NLD.14141             42           42            42
#> 
#> , , type.species.color = animal.magpie.white
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084             42           42            42
#>   6p25.53p25.NLD.14113             42           42            42
#>   6p75.53p25.NLD.14141             42           42            42
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084             42           42            42
#>   6p25.53p25.NLD.14113             42           42            42
#>   6p75.53p25.NLD.14141             42           42            42
#> 
```
