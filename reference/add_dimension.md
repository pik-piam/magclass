# add_dimension

This function adds a named dimension as dimension number "dim" with the
name "add" with an empty data column with the name "nm". Please start
using this function's successor [`addDim`](addDim.md). add_dimension
might eventually become deprecated.

## Usage

``` r
add_dimension(x, dim = 3.1, add = NULL, nm = "dummy", expand = TRUE)
```

## Arguments

- x:

  MAgPIE object which should be extended.

- dim:

  The dimension number of the new dimension (e.g. 3.1)

- add:

  The name of the new dimension

- nm:

  One or more names of items in the new dimension.

- expand:

  If TRUE, each item from the item argument is added to each item
  already present, resulting in e.g. \`c("A.d1", "B.d1", "A.d2",
  "B.d2")\`. Otherwise, length of item must equal the number of items
  already present and they are simply added, resulting in e.g.
  \`c("A.d1", "B.d2")\`.

## Value

The extended MAgPIE object

## See also

[`add_columns`](add_columns.md),[`mbind`](mbind.md)

Other DimensionManipulation: [`add_columns()`](add_columns.md),
[`collapseDim()`](collapseDim.md),
[`collapseNames()`](collapseNames.md), [`dimCode()`](dimCode.md),
[`dimOrder()`](dimOrder.md), [`dimReduce()`](dimReduce.md)

## Author

Jan Philipp Dietrich, Benjamin Bodirsky, Pascal Sauer

## Examples

``` r
a <- maxample("animal")
str(addDim(a, dim = 3.2))
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:1750] 0 0 8 3 4 3 8 7 3 6 ...
#>  $ dimnames         :List of 3
#>   ..$ x.y.country.cell      : chr [1:35] "5p75.53p25.NLD.14084" "6p25.53p25.NLD.14113" "6p75.53p25.NLD.14141" "4p75.52p75.NLD.14040" ...
#>   ..$ year.month.day        : chr [1:10] "y2000.april.20" "y2000.may.20" "y2000.june.20" "y2001.april.20" ...
#>   ..$ type.new.species.color: chr [1:5] "animal.dummy.rabbit.black" "animal.dummy.rabbit.white" "animal.dummy.bird.black" "animal.dummy.bird.red" ...
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
str(addDim(a, dim = 2.3, item = paste0("d", 1:3)))
#> A magpie object (package: magclass)
#>  @ .Data:  num [1:5250] 0 0 8 3 4 3 8 7 3 6 ...
#>  $ dimnames         :List of 3
#>   ..$ x.y.country.cell  : chr [1:35] "5p75.53p25.NLD.14084" "6p25.53p25.NLD.14113" "6p75.53p25.NLD.14141" "4p75.52p75.NLD.14040" ...
#>   ..$ year.month.new.day: chr [1:30] "y2000.april.d1.20" "y2000.may.d1.20" "y2000.june.d1.20" "y2001.april.d1.20" ...
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
```
