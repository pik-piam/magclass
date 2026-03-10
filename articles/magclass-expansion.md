# MAGPIE Class Object Expansion

One of the most important features of magclass objects is their auto
expansion when used in calculations with other magclass objects:
Dimensions of the objects in the calculation are compared and tried to
be matched accordingly. Afterwards all objects are expanded in
dimensionality to the superset of dimensions involved in the
calculation.

This tutorial will go through different variants of this auto expansion
logic and explain the differences.

## Auto expansion v1 vs v2

This auto expansion can follow different rules. In magclass version 1 to
4 it was assumed that there is exactly one spatial and one temporal
dimension, but various data dimensions (auto expand version 1).
Beginning with version 5 of the package this logic has been harmonized
and it is now assumed for all main dimensions (spatial, temporal, data)
that they consist of more than one subdimension (auto expand version 2).
With the updated auto expansion calculation, which previously have led
to an error, will now work:

Creating two magpie objects with different world regions:

``` r
library(magclass)
#> 
#> Attaching package: 'magclass'
#> The following objects are masked from 'package:base':
#> 
#>     pmax, pmin
a <- b <- maxample("pop")
getItems(b, dim = 1)[1] <- "AAA"
```

The multiplication for auto expand version 1 (magclass \< 5.0) failed,
but the same calculation with auto expand version 2.1 (magclass \>= 5.0)
works:

``` r

options(magclass_expand_version = 2.1) # default setting for magclass >= 5.0
head(a * b)
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>          t
#> i.i1         y1995    y2005     y2015     y2025     y2035   y2045
#>   AFR.AAA 305440.1 485028.7  790641.1 1263623.3 1930237.7 2754704
#>   AFR.CPA 707763.9 995581.9 1350184.2 1789684.2 2279820.9 2774969
#>   AFR.EUR 306419.5 405578.8  527959.5  680390.1  853854.4 1027323
#> 
#> , , scenario = B1
#> 
#>          t
#> i.i1         y1995     y2005     y2015     y2025     y2035     y2045
#>   AFR.AAA 305440.1  521067.4  868698.5 1250661.9 1606125.2 1913352.9
#>   AFR.CPA 707763.9 1031711.3 1397817.6 1712297.4 1924732.0 2024620.8
#>   AFR.EUR 306419.5  423877.5  562607.3  686632.2  785085.5  853625.1
```

As this modification only changes cases which did not work previously it
could be included without causing backwards compatibility issues.

## Set matching

Another way of modifying auto expansion in magclass is the use of set
matching. By default dimensions are compared based on their elements: If
dimensions are found which contain the same elements it is assumed that
it is the same dimension. While this assumption is correct in many cases
it fails in some others, e.g. if an import-export matrix should be
created. A better way of distinguishing whether dimensions are identical
or not is the use of the set names (the names of the dimension). If they
are identical it can be assumed that it is the same dimension otherwise
it can be assumed that it is not.

Due to the absense of set names in initial versions of magclass the
dimension matching was initially implement based on element comparison.
With version 5 of the package set matching is now also supported but
switched off by default due to backwards compatibility issues. If you
want to switch it on (recommended) you can do so by setting the option
`options(magclass_setMatching=TRUE)`. However, if you do so be prepared
that the package behavior will be quite different in many cases as the
following examples show:

### Idential set elements, different set names

Without set matching, identical set elements but different set names
produce a simple entity matching without expansion:

``` r
options(magclass_expand_version = 2.1) # default setting for magclass >= 5.0

a <- b <- maxample("pop")

getSets(a)[1] <- "import"
getSets(b)[1] <- "export"

options(magclass_setMatching = FALSE)
head(a * b)
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>       t
#> import     y1995     y2005     y2015     y2025     y2035     y2045
#>    AFR  305440.1  485028.7  790641.1 1263623.3 1930237.7 2754703.6
#>    CPA 1640026.0 2043556.1 2305720.7 2534750.5 2692716.7 2795383.2
#>    EUR  307402.0  339143.2  352550.9  366351.8  377708.6  383123.8
#> 
#> , , scenario = B1
#> 
#>       t
#> import     y1995     y2005     y2015     y2025     y2035     y2045
#>    AFR  305440.1  521067.4  868698.5 1250661.9 1606125.2 1913352.9
#>    CPA 1640026.0 2042784.2 2249220.0 2344328.4 2306540.8 2142359.3
#>    EUR  307402.0  344815.6  364369.2  376971.4  383755.4  380837.1
```

With set matching the same calculation will expand the spatial dimension
to a cross product of each region with each other:

``` r
options(magclass_setMatching = TRUE)
head(a * b)
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>              t
#> import.export    y1995    y2005     y2015     y2025     y2035   y2045
#>       AFR.AFR 305440.1 485028.7  790641.1 1263623.3 1930237.7 2754704
#>       AFR.CPA 707763.9 995581.9 1350184.2 1789684.2 2279820.9 2774969
#>       AFR.EUR 306419.5 405578.8  527959.5  680390.1  853854.4 1027323
#> 
#> , , scenario = B1
#> 
#>              t
#> import.export    y1995     y2005     y2015     y2025     y2035
#>       AFR.AFR 305440.1  521067.4  868698.5 1250661.9 1606125.2
#>       AFR.CPA 707763.9 1031711.3 1397817.6 1712297.4 1924732.0
#>       AFR.EUR 306419.5  423877.5  562607.3  686632.2  785085.5
#>              t
#> import.export     y2045
#>       AFR.AFR 1913352.9
#>       AFR.CPA 2024620.8
#>       AFR.EUR  853625.1
```

### Different set element, identical set names

If the set elements are different but the set names identical this will
lead to an expansion of the spatial dimension in the case without set
matching (as the set name is just ignored):

``` r
options(magclass_expand_version = 2.1) # default setting for magclass >= 5.0

a <- b <- maxample("pop")

getItems(a, dim = 1)[1] <- "AAA"
getItems(b, dim = 1)[1] <- "BBB"

options(magclass_setMatching = FALSE)
head(a * b)
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>          t
#> i.i1         y1995    y2005     y2015     y2025     y2035   y2045
#>   AAA.BBB 305440.1 485028.7  790641.1 1263623.3 1930237.7 2754704
#>   AAA.CPA 707763.9 995581.9 1350184.2 1789684.2 2279820.9 2774969
#>   AAA.EUR 306419.5 405578.8  527959.5  680390.1  853854.4 1027323
#> 
#> , , scenario = B1
#> 
#>          t
#> i.i1         y1995     y2005     y2015     y2025     y2035     y2045
#>   AAA.BBB 305440.1  521067.4  868698.5 1250661.9 1606125.2 1913352.9
#>   AAA.CPA 707763.9 1031711.3 1397817.6 1712297.4 1924732.0 2024620.8
#>   AAA.EUR 306419.5  423877.5  562607.3  686632.2  785085.5  853625.1
```

With set matching it will return an error as identical set names but
differing elements indicates most likely an error.

``` r
options(magclass_setMatching = TRUE)
head(a * b)
#> Error in `magpie_expand_dim()`:
#> !  Identical set names but different content. Correct set names!
```

To ensure that existing calculations continue to work as they did before
use `options(magclass_setMatching=FALSE)`, to write new code more
consistently with magclass objects use
`options(magclass_setMatching=TRUE)`.
