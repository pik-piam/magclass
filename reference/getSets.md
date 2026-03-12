# Get sets

Extracts sets of a MAgPIE-object if available

## Usage

``` r
getSets(x, fulldim = TRUE, sep = ".")

getSets(x, fulldim = TRUE, sep = ".") <- value
```

## Arguments

- x:

  MAgPIE object

- fulldim:

  bool: Consider dimension 3 as a possible aggregate of more dimensions
  (TRUE) or stick to it as one dimension (FALSE)

- sep:

  A character separating joined dimension names

- value:

  A vector with set names you want to replace the current set names of
  the object with.

## Value

Sets of the MAgPIE-object. If no information about contained sets is
available NULL

## Functions

- `getSets(x, fulldim = TRUE, sep = ".") <- value`: replace set names

## See also

[`getRegions`](getRegions.md),
[`getNames`](getNames.md),[`getYears`](getYears.md),
[`getCPR`](getCPR.md), [`read.magpie`](read.magpie.md),
[`write.magpie`](write.magpie.md), `"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getNames()`](getNames.md),
[`getRegionList()`](getRegionList.md), [`getRegions()`](getRegions.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Markus Bonsch, Jan Philipp Dietrich

## Examples

``` r
a <- new.magpie("GLO.1", 2000, c("a.o1", "b.o1", "a.o2"))
 getSets(a) <- c("reg", "cell", "t", "bla", "blub")
 getSets(a)
#>   d1.1   d1.2   d2.1   d3.1   d3.2 
#>  "reg" "cell"    "t"  "bla" "blub" 

 getSets(a)["d3.1"] <- "BLA"
 getSets(a, fulldim = FALSE)
#> [1] "reg.cell" "t"        "BLA.blub"
 getSets(a)
#>   d1.1   d1.2   d2.1   d3.1   d3.2 
#>  "reg" "cell"    "t"  "BLA" "blub" 
```
