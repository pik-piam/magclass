# maxample

A collection of magclass example data sets

## Usage

``` r
maxample(data)
```

## Arguments

- data:

  name of the example data set. Currently available are "pop" (regional
  population data, previously named "population_magpie"), "animal"
  (fictional, high-dimensional animal sighting data set) and "bilateral"
  (fictional, bilateral trade cost data set).

## Value

the chosen example data set

## See also

Other Display: [`head.magpie()`](head.magpie.md), [`mplot()`](mplot.md),
[`print.magpie()`](print.magpie.md), [`show-methods`](show-methods.md),
[`str.magpie()`](str.magpie.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
p <- maxample("pop")
str(p)
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:320] 553 1281 554 276 452 ...
#>  $ dimnames:List of 3
#>   ..$ i       : chr [1:10] "AFR" "CPA" "EUR" "FSU" ...
#>   ..$ t       : chr [1:16] "y1995" "y2005" "y2015" "y2025" ...
#>   ..$ scenario: chr [1:2] "A2" "B1"
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"

a <- maxample("animal")
str(a)
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:1750] 0 0 8 3 4 3 8 7 3 6 ...
#>  $ dimnames         :List of 3
#>   ..$ x.y.country.cell  : chr [1:35] "5p75.53p25.NLD.14084" "6p25.53p25.NLD.14113" "6p75.53p25.NLD.14141" "4p75.52p75.NLD.14040" ...
#>   ..$ year.month.day    : chr [1:10] "y2000.april.20" "y2000.may.20" "y2000.june.20" "y2001.april.20" ...
#>   ..$ type.species.color: chr [1:5] "animal.rabbit.black" "animal.rabbit.white" "animal.bird.black" "animal.bird.red" ...
#>  $ .internal.selfref:<externalptr> 
#>  $ Metadata         :List of 4
#>   ..$ unit       : 'units' num 1
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr(0) 
#>   .. .. ..$ denominator: chr [1:2] "km" "km"
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user       : chr "jpd"
#>   ..$ date       : chr "Wed Jan 27 09:31:23 2021"
#>   ..$ description: chr "Fictious species observation data set characterized by its high amount of sub-dimensions which makes it useful "| __truncated__
getItems(a, split = TRUE)
#> [[1]]
#> [[1]]$x
#> [1] "5p75" "6p25" "6p75" "4p75" "5p25" "4p25" "3p25" "3p75"
#> 
#> [[1]]$y
#> [1] "53p25" "52p75" "52p25" "51p75" "51p25" "50p75" "50p25" "49p75"
#> 
#> [[1]]$country
#> [1] "NLD" "BEL" "LUX"
#> 
#> [[1]]$cell
#>  [1] "14084" "14113" "14141" "14040" "14083" "14112" "14140" "14039"
#>  [9] "14058" "14082" "14111" "14139" "14021" "14038" "14057" "14081"
#> [17] "13988" "14004" "14020" "14037" "14056" "14080" "13987" "14003"
#> [25] "14019" "14036" "14055" "14079" "14018" "14035" "14054" "14078"
#> [33] "14053" "14077" "14106"
#> 
#> 
#> [[2]]
#> [[2]]$year
#> [1] "y2000" "y2001" "y2002"
#> 
#> [[2]]$month
#> [1] "april"  "may"    "june"   "august"
#> 
#> [[2]]$day
#> [1] "20"
#> 
#> 
#> [[3]]
#> [[3]]$type
#> [1] "animal"
#> 
#> [[3]]$species
#> [1] "rabbit" "bird"   "dog"   
#> 
#> [[3]]$color
#> [1] "black" "white" "red"   "brown"
#> 
#> 
```
