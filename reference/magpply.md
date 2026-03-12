# magpply

apply command for magpieobjects. Very efficient for replacing loops.

## Usage

``` r
magpply(X, FUN, MARGIN = NULL, DIM = NULL, ..., INTEGRATE = FALSE)
```

## Arguments

- X:

  magpie object

- FUN:

  function that shall be applied X

- MARGIN:

  dimension over which FUN shall be applied (like a loop over that
  dimension). This dimension will be preserved in the output object (see
  also `DIM`).

- DIM:

  dimension in which FUN shall be applied. This dimension will be
  missing in the output. DIM and MARGIN are opposite ways of expressing
  the dimensions to be addressed and you must only use one of them with
  MARGIN excluding dimensions from the calculation and DIM including
  them.

- ...:

  further parameters passed on to FUN

- INTEGRATE:

  if TRUE, the output will be filled into an magpie object of the same
  dimensionality as X

## Value

magpie object

## See also

Other SelectionCalculation: [`mcalc()`](mcalc.md),
[`mselect()`](mselect.md), [`where()`](where.md)

## Author

Jan Philipp Dietrich, Benjamin Leon Bodirsky

## Examples

``` r
pop <- maxample("pop")
magpply(pop, FUN = sum, MARGIN = 2)
#>    y1995    y2005    y2015    y2025    y2035    y2045    y2055 
#> 10945.73 12941.31 14601.30 16162.53 17486.73 18513.59 19239.70 
#>    y2065    y2075    y2085    y2095    y2105    y2115    y2125 
#> 19692.97 19913.84 19916.29 19651.19 19442.44 19442.44 19442.44 
#>    y2135    y2145 
#> 19442.44 19442.44 
fourdim <- pop * setNames(pop, c("jkk", "lk"))
magpply(fourdim, FUN = sum, MARGIN = c(1, 3.1))
#>      scenario
#> i              A2          B1
#>   AFR 113715256.6  67331000.8
#>   CPA  81044791.1  54681554.1
#>   EUR  11929603.7  11073225.0
#>   FSU   3053526.5   2677321.6
#>   LAM  24463605.8  16600126.7
#>   MEA  23532412.9  19283320.0
#>   NAM   6510498.8   6435064.0
#>   PAO    790726.7    725689.4
#>   PAS  16253213.3  12311010.5
#>   SAS 183373724.0 120146934.2
```
