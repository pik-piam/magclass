# Get cells per region

Counts how often each element of the provided subdimension exists in the
given data set. Originally created to count the number of cells in a
region (this is also where its name originates from) it can now be used
to count elements of any subdimension via the dim argument.

## Usage

``` r
getCPR(x, dim = 1.1)
```

## Arguments

- x:

  MAgPIE object or a resolution written as numeric (currently only data
  for 0.5 degree resolution is available).

- dim:

  Dimension for which the items should be returned. Either number or
  name of dimension or a vector of these (in case of a vector all
  subimensions must belong to the same main dimension!). See
  [`dimCode`](dimCode.md) for more details.

## Value

cells per region

## See also

[`getRegions`](getRegions.md), [`read.magpie`](read.magpie.md),
[`write.magpie`](write.magpie.md)

Other GetterSetter: [`getCells()`](getCells.md),
[`getComment()`](getComment.md), [`getCoords()`](getCoords.md),
[`getDim()`](getDim.md), [`getItems()`](getItems.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
getCPR(0.5)
#>   AFR   CPA   EUR   FSU   LAM   MEA   NAM   PAO   PAS   SAS 
#>  8224  4795  3448 14048  7226  3957 10985  3124  1326  2066 
a <- maxample("animal")
getCPR(a, dim = "color")
#> black brown   red white 
#>     2     1     1     1 
getCPR(a, dim = 3.2)
#>   bird    dog rabbit 
#>      2      1      2 
getCPR(a, dim = "country")
#> BEL LUX NLD 
#>  17   1  17 
getCPR(a, dim = c("color", "species"))
#>       bird dog rabbit
#> black    1   0      1
#> brown    0   1      0
#> red      1   0      0
#> white    0   0      1
```
