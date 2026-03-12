# getComment

Extracts the comment from a MAgPIE-object

## Usage

``` r
getComment(x)

getComment(x) <- value

setComment(object, nm = NULL)
```

## Arguments

- x, object:

  MAgPIE object

- value, nm:

  A vector containing the comment.

## Value

getComment returns the comment attached to a MAgPIE-object, NULL if no
comment is present. setComment returns the magpie object with the
modified comment.

## Functions

- `getComment(x) <- value`: set comment

- `setComment()`: set comment

## See also

[`getRegions`](getRegions.md), [`getNames`](getNames.md),
[`getYears`](getYears.md), [`getCPR`](getCPR.md),
[`read.magpie`](read.magpie.md), [`write.magpie`](write.magpie.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getCoords()`](getCoords.md),
[`getDim()`](getDim.md), [`getItems()`](getItems.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Markus Bonsch

## Examples

``` r
a <- as.magpie(1)
 # returns NULL
 getComment(a)
#> NULL
 # set the comment
 getComment(a) <- c("bla", "blubb")
 getComment(a)
#> [1] "bla"   "blubb"
```
