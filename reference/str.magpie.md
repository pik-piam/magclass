# str

str method for MAgPIE objects for conventient display of the structure
of a magpie object.

## Usage

``` r
# S3 method for class 'magpie'
str(object, ...)
```

## Arguments

- object:

  MAgPIE object

- ...:

  arguments to be passed to or from other methods.

## Details

In contrast to the default str this will not show the attributes of
object@.Data as these contain only a duplicate of dimnames. Also, dim is
not shown, because the information it provides is implicitly included in
dimnames.

## See also

[`str`](https://rdrr.io/r/utils/str.html)

Other Display: [`head.magpie()`](head.magpie.md),
[`maxample()`](maxample.md), [`mplot()`](mplot.md),
[`print.magpie()`](print.magpie.md), [`show-methods`](show-methods.md)

## Author

Pascal Sauer

## Examples

``` r
str(maxample("pop"))
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
```
