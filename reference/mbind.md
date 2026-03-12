# mbind

Merges MAgPIE-objects with identical structure in two dimensions. If
data differs in the temporal or spatial dimension each year or
region/cell must appear only once!

## Usage

``` r
mbind(...)
```

## Arguments

- ...:

  MAgPIE objects or a list of MAgPIE objects that should be merged.

## Value

The merged MAgPIE object

## See also

`"`[`magpie`](magpie-class.md)`"`

Other DataBinding: [`cbind.magpie()`](cbind.magpie.md),
[`extend()`](extend.md), [`magpie_expand()`](magpie_expand.md),
[`magpiesort()`](magpiesort.md), [`matchDim()`](matchDim.md)

## Author

Jan Philipp Dietrich, Misko Stevanovic

## Examples

``` r
m <- new.magpie(c("AFR", "CPA", "EUR"), c(1995, 2005), "Data1", fill = c(1, 2, 3, 4, 5, 6))
ms <- dimSums(m, dim = 3.1)
mbind(m, ms)
#> , , data = Data1
#> 
#>       year
#> region y1995 y2005
#>    AFR     1     4
#>    CPA     2     5
#>    EUR     3     6
#> 
#> , , data = dummy
#> 
#>       year
#> region y1995 y2005
#>    AFR     1     4
#>    CPA     2     5
#>    EUR     3     6
#> 
my <- new.magpie(getRegions(m), 2010, getNames(m), fill = c(6, 6, 4))
mbind(m, my)
#>       year
#> region y1995 y2005 y2010
#>    AFR     1     4     6
#>    CPA     2     5     6
#>    EUR     3     6     4
md <- new.magpie(getRegions(m), getYears(m), "Data2", fill = c(7, 6, 5, 7, 8, 9))
mbind(m, md)
#> , , data = Data1
#> 
#>       year
#> region y1995 y2005
#>    AFR     1     4
#>    CPA     2     5
#>    EUR     3     6
#> 
#> , , data = Data2
#> 
#>       year
#> region y1995 y2005
#>    AFR     7     7
#>    CPA     6     8
#>    EUR     5     9
#> 

pop <- maxample("pop")
a <- mbind(pop, pop)
dim(pop)
#> [1] 10 16  2
dim(a)
#> [1] 10 16  4
```
