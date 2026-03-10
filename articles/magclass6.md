# magclass 6

The step from magclass 5.29 to 6.00 marks a major rewrite of the whole
package. While the definition of the magclass object has been refined
and generalized over time (allowing multiple sub-dimensions in all main
dimensions, introduction of set names, etc.) some functions of the
package still relied on implicit assumptions which were not necessarily
valid anymore for the new definitions. This led to situations in which
functions might only work for specific cases (such as functions for sub
dimension manipulation only working in the third dimension) or even
return wrong results in some instances.

This tutorial will go through the major changes coming with version 6
and what they mean for working with magclass objects.

## Testing

Magclass comes now with more than 600 unit tests which are covering more
than 90% lines of code of the total package. These tests should help to
detect code breaking changes already early in the process and reduce the
risk of pushing incompatible modifications to our main repository. In
addition, it is planned to expand the test collection with every new
feature or newly observed bug so that if issues arise that they only
arise once.

## (soon to be) deprecated functions

There are some function in magclass building on the old standard of
having exactly one spatial and one temporal dimension (no
subdimensions), which has been generalized in the meantime (up to 9
subdimension in every of the three main dimensions allowed). So far only
two of them received a deprecation warning, which are `fulldim` (does
not split subdimensions in first two dimensions) and `getRegionList`
(assumes to only have regions in the first dimension). Functionality can
now be found in the new function `getItems` which allows to return
elements of (sub-)dimensions in various ways:

``` r
library(magclass)
#> 
#> Attaching package: 'magclass'
#> The following objects are masked from 'package:base':
#> 
#>     pmax, pmin
a <- maxample("animal")
p <- maxample("pop")
```

instead of `fulldim(a)` to receive a list of all sub-dimensions use now:

``` r
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
```

In contrast to `fulldim` this splits all dimensions in sub-dimensions
and reports the corresponding set names.

It is also possible to split a specific main dimension into
sub-dimensions:

``` r
getItems(a, dim = 1, split = TRUE)
#> $x
#> [1] "5p75" "6p25" "6p75" "4p75" "5p25" "4p25" "3p25" "3p75"
#> 
#> $y
#> [1] "53p25" "52p75" "52p25" "51p75" "51p25" "50p75" "50p25" "49p75"
#> 
#> $country
#> [1] "NLD" "BEL" "LUX"
#> 
#> $cell
#>  [1] "14084" "14113" "14141" "14040" "14083" "14112" "14140" "14039"
#>  [9] "14058" "14082" "14111" "14139" "14021" "14038" "14057" "14081"
#> [17] "13988" "14004" "14020" "14037" "14056" "14080" "13987" "14003"
#> [25] "14019" "14036" "14055" "14079" "14018" "14035" "14054" "14078"
#> [33] "14053" "14077" "14106"
```

Instead of `getRegionList(a)` one should use `getItems` as well:

``` r
getItems(a, dim = 1.1, full = TRUE)
#>  [1] "5p75" "6p25" "6p75" "4p75" "5p75" "6p25" "6p75" "4p75" "5p25"
#> [10] "5p75" "6p25" "6p75" "4p25" "4p75" "5p25" "5p75" "3p25" "3p75"
#> [19] "4p25" "4p75" "5p25" "5p75" "3p25" "3p75" "4p25" "4p75" "5p25"
#> [28] "5p75" "4p25" "4p75" "5p25" "5p75" "5p25" "5p75" "6p25"
```

This command gives the same results as `getRegionList` but makes it more
obvious what the command is doing (returning the first spatial
sub-dimension in full length)

Some other functions such as `getRegions` or `getCells` have not been
formally deprecated due to that broad usage, but are encouraged to be
replaced as well with `getItems`.

Another function which recieved a deprecation warning is
`write.report2`. So far the package contained two very similar functions
`write.report` and `write.report2` from which the latter is the more
efficient one. In the new version the old `write.report` function has
been replaced with the code of `write.report2` so that in the future all
computation are based on the same function.

## New feature: extended sub-dimension support

So far `dimSums`, `add_columns`, `addDim` were only functioning for the
sub-dimensions in the third dimension while `getCPR` were only
functioning for the first dimension. All these functions have been
completely rewritten and work now for all sub-dimensions and are able to
address a dimension either by its `dimCode` or set name:

``` r
dimSums(a, c("x", "y", "cell", "day", "month", "species", "color"))
#> An object of class "magpie"
#> , , type = animal
#> 
#>        year
#> country y2000 y2001 y2002
#>     NLD  1577  2594  8339
#>     BEL  1550  2529  8126
#>     LUX   124   200   731

getCPR(a, 3.2)
#>   bird    dog rabbit 
#>      2      1      2

addDim(p[, 1, 1], 1.2)
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>            t
#> i.new           y1995
#>   AFR.dummy  552.6664
#>   CPA.dummy 1280.6350
#>   EUR.dummy  554.4384
#>   FSU.dummy  276.3431
#>   LAM.dummy  451.9981
#>   MEA.dummy  277.7437
#>   NAM.dummy  292.1132
#>   PAO.dummy  133.7772
#>   PAS.dummy  383.2277
#>   SAS.dummy 1269.9243

add_columns(p[, 1, 1], dim = "i")
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>      t
#> i         y1995
#>   AFR  552.6664
#>   CPA 1280.6350
#>   EUR  554.4384
#>   FSU  276.3431
#>   LAM  451.9981
#>   MEA  277.7437
#>   NAM  292.1132
#>   PAO  133.7772
#>   PAS  383.2277
#>   SAS 1269.9243
#>   new        NA
```

## New magclass subsetting variant

To ease the process to write generalized functions which work for all
main dimensions a new sub-setting option has been introduced which
allows to specify the main dimension in which the sub-setting should
take place via a new `dim` argument:

``` r
p[3, dim = 2] # equivalent to p[,3,]
#> An object of class "magpie"
#> , , scenario = A2
#> 
#>      t
#> i       y2015
#>   AFR  889.18
#>   CPA 1518.46
#>   EUR  593.76
#>   FSU  302.62
#>   LAM  646.02
#>   MEA  489.22
#>   NAM  353.25
#>   PAO  155.27
#>   PAS  604.94
#>   SAS 1796.76
#> 
#> , , scenario = B1
#> 
#>      t
#> i       y2015
#>   AFR  932.04
#>   CPA 1499.74
#>   EUR  603.63
#>   FSU  305.26
#>   LAM  623.20
#>   MEA  502.51
#>   NAM  349.85
#>   PAO  157.37
#>   PAS  590.42
#>   SAS 1687.80
```

## Generalized handling of spatial raster data

Previously spatial data could only be handled for a pre-defined set of
59199 0.5x0.5 degree cells for which cells were numbered from 1 to 59199
and coordinates had to be known by the user and properly assigned to
these cells. With the update raster data instead gets assigned its
coordinates in the spatial dimension allowing also to handle data for
differing resolutions and/or coverage. Accordingly, `read.magpie` and
`write.magpie` have been adapted to this new behavior and custom
implemetations for `ncdf4` and `asc` file support have been replaced
with the read/write routines provided by the `raster` package. For
applications this means that the behavior of code reading/writing these
files might change and might require some modifications, but for the
future it should ease the handling and exchange of spatial data with
magclass objects.

## One-stop-shop-function getItems

As indicated earlier `getItems` can be used to replace `fulldim` and
`getRegionList`. But its functionality goes beyond it making it the
function to be used when elements of (sub-)dimensions should be read,
added, removed or modified. While so far many different functions had to
be remembered for these applications (such as `getCells`, `getNames`,
`addDim`) it is now recommended to first look at `getItems`. In the
following some examples of its functionality:

Returning a sub-dimension:

``` r
getItems(a, dim = 3.2)
#> [1] "rabbit" "bird"   "dog"
getItems(a, dim = "color")
#> [1] "black" "white" "red"   "brown"
```

Returning the full vector of a sub-dimension:

``` r
getItems(a, dim = 3.2, full = TRUE)
#> [1] "rabbit" "rabbit" "bird"   "bird"   "dog"
```

Returning a full main dimension…

``` r
getItems(a, dim = 3)
#> [1] "animal.rabbit.black" "animal.rabbit.white" "animal.bird.black"  
#> [4] "animal.bird.red"     "animal.dog.brown"
```

…split into its sub-dimensions

``` r
getItems(a, dim = 3, split = TRUE)
#> $type
#> [1] "animal"
#> 
#> $species
#> [1] "rabbit" "bird"   "dog"   
#> 
#> $color
#> [1] "black" "white" "red"   "brown"
```

Replace elements element-wise:

``` r
getItems(a, dim = "color") <- paste0("color", 1:4)
getItems(a, dim = 3)
#> [1] "animal.rabbit.color1" "animal.rabbit.color2"
#> [3] "animal.bird.color1"   "animal.bird.color3"  
#> [5] "animal.dog.color4"
```

Replace the whole vector:

``` r
getItems(a, dim = "color", full = TRUE) <- paste0("color", 1:5)
getItems(a, dim = 3)
#> [1] "animal.rabbit.color1" "animal.rabbit.color2"
#> [3] "animal.bird.color3"   "animal.bird.color4"  
#> [5] "animal.dog.color5"
```

Delete a dimension:

``` r
getItems(a, dim = "color") <- NULL
getItems(a, dim = 3)
#> [1] "animal.rabbit" "animal.rabbit" "animal.bird"   "animal.bird"  
#> [5] "animal.dog"
```

Add a dimension:

``` r
getItems(a, maindim = 3, dim = "newcolor") <- paste0("color", 1:5)
getItems(a, dim = 3)
#> [1] "animal.rabbit.color1" "animal.rabbit.color2"
#> [3] "animal.bird.color3"   "animal.bird.color4"  
#> [5] "animal.dog.color5"
```

All these manipulations work for all (sub-)dimensions of the object.

## Internal code modifications

As the complete package underwent a code review also many other
functions which have not been mentioned here received modifications.
While this should affect the behavior of these functions it improved in
some cases the efficiency of the underlying computations.
