# dimReduce

Remove dimensions which contain identical data for all elements in it

## Usage

``` r
dimReduce(x, dim_exclude = NULL)
```

## Arguments

- x:

  MAgPIE object which should be reduced

- dim_exclude:

  Vector with names of dimensions which must not be reduced

## Value

The reduced MAgPIE object

## Note

This function has some similarities to [`collapseDim`](collapseDim.md),
but serves a different purpose. While [`collapseDim`](collapseDim.md)
only removes dimensions which contain only a single element or which it
is specifically told to remove, `dimReduce` looks whether the entries of
a multi-entry dimension are all the same and removes dimensions for
which this is the case. In some cases both will lead to the same result
but in many other cases the results will differ.

## See also

[`addDim`](addDim.md)

Other DimensionManipulation: [`addDim()`](addDim.md),
[`add_columns()`](add_columns.md), [`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimOrder()`](dimOrder.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# create data with 5 identical scenarios
p <- addDim(maxample("pop")[1:3, 1:3, ], item = paste0("scen", 1:2))
str(p)
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:36] 553 1281 554 696 1430 ...
#>  $ dimnames:List of 3
#>   ..$ i           : chr [1:3] "AFR" "CPA" "EUR"
#>   ..$ t           : chr [1:3] "y1995" "y2005" "y2015"
#>   ..$ new.scenario: chr [1:4] "scen1.A2" "scen1.B1" "scen2.A2" "scen2.B1"
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"
str(dimReduce(p))
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:18] 553 1281 554 696 1430 ...
#>  $ dimnames:List of 3
#>   ..$ i       : chr [1:3] "AFR" "CPA" "EUR"
#>   ..$ t       : chr [1:3] "y1995" "y2005" "y2015"
#>   ..$ scenario: chr [1:2] "A2" "B1"
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"

# set years to same value
p[, , ] <- setYears(p[, 1, ], NULL)
str(p)
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:36] 553 1281 554 553 1281 ...
#>  $ dimnames:List of 3
#>   ..$ i           : chr [1:3] "AFR" "CPA" "EUR"
#>   ..$ t           : chr [1:3] "y1995" "y2005" "y2015"
#>   ..$ new.scenario: chr [1:4] "scen1.A2" "scen1.B1" "scen2.A2" "scen2.B1"
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"
str(dimReduce(p))
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:3] 553 1281 554
#>  $ dimnames:List of 3
#>   ..$ i : chr [1:3] "AFR" "CPA" "EUR"
#>   ..$ d2: NULL
#>   ..$   : NULL
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"

# set regions to same value
p[, , ] <- setCells(p[1, , ], "GLO")
str(p)
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:36] 553 553 553 553 553 ...
#>  $ dimnames:List of 3
#>   ..$ i           : chr [1:3] "AFR" "CPA" "EUR"
#>   ..$ t           : chr [1:3] "y1995" "y2005" "y2015"
#>   ..$ new.scenario: chr [1:4] "scen1.A2" "scen1.B1" "scen2.A2" "scen2.B1"
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"
str(dimReduce(p))
#> A magpie object (package: magclass)
#>  @ .Data:  num 553
#>  $ dimnames:List of 3
#>   ..$ d1: chr "GLO"
#>   ..$ d2: NULL
#>   ..$   : NULL
#>  $ Metadata:List of 3
#>   ..$ unit: 'units' num 1e+06
#>   .. ..- attr(*, "units")=List of 2
#>   .. .. ..$ numerator  : chr "people"
#>   .. .. ..$ denominator: chr(0) 
#>   .. .. ..- attr(*, "class")= chr "symbolic_units"
#>   ..$ user: chr "jpd"
#>   ..$ date: chr "2018-01-15 14:19:27"
```
