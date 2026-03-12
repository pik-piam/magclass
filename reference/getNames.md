# Get dataset names

Extracts dataset names of a MAgPIE-object

## Usage

``` r
getNames(x, fulldim = FALSE, dim = NULL)

getNames(x, dim = NULL) <- value
```

## Arguments

- x:

  MAgPIE object

- fulldim:

  specifies, how the object is treated. In case of FALSE, it is assumed
  that x is 3 dimensional and dimnames(x)\[\[3\]\] is returned. In case
  of TRUE, the dimnames of the real third dimension namesare returned

- dim:

  Argument to choose a specific data dimension either by name of the
  dimension or by number of the data dimension.

- value:

  a vector of names current names should be replaced with. If only one
  data element exists you can also set the name to NULL.

## Value

getNames returns data names of the MAgPIE-object, whereas setNames
returns the MAgPIE object with the manipulated data names.

## Details

setNames is a shortcut to use a MAgPIE object with manipulated data
names. The setNames method uses the variable names "object" and "nm" in
order to be consistent to the already existing function setNames.

## Functions

- `getNames(x, dim = NULL) <- value`: set names

## See also

[`setNames-methods`](setNames-methods.md),
[`getRegions`](getRegions.md), [`getYears`](getYears.md),
[`getCPR`](getCPR.md), [`read.magpie`](read.magpie.md),
[`write.magpie`](write.magpie.md),[`ndata`](ncells.md),
`"`[`magpie`](magpie-class.md)`"`

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getItems()`](getItems.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- as.magpie(1)
getNames(a)
#> NULL
setNames(a, "bla")
#> [1] 1

x <- new.magpie("GLO", 2000, c("a.o1", "b.o1", "a.o2"))
getNames(x, dim = 2)
#> [1] "o1" "o2"

getSets(x, fulldim = FALSE)[3] <- "bla.blub"
getNames(x, dim = "bla")
#> [1] "a" "b"

getSets(x)[4] <- "ble"
getNames(x, dim = "ble") <- c("Hi", "Bye")
x
#>  a.Hi  b.Hi a.Bye 
#>    NA    NA    NA 
```
