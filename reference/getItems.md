# Get Items

Extract items of a given (sub-)dimension of a MAgPIE-object

## Usage

``` r
getItems(x, dim = NULL, split = FALSE, full = FALSE)

getItems(x, dim, full = NULL, maindim = NULL, raw = FALSE) <- value
```

## Arguments

- x:

  MAgPIE object

- dim:

  Dimension for which the items should be returned. Either number or
  name of dimension or a vector of these. See [`dimCode`](dimCode.md)
  for more details.

- split:

  Boolean which determines whether a main dimension should be split in
  subdimensions. Only applicable to main dimensions (1,2,3) and ignored
  for all other.

- full:

  if TRUE dimension names are returned as they are (including
  repetitions), if FALSE only the dimension elements (unique list of
  entries) are returned.

- maindim:

  main dimension the data should be added to (does not need to be set if
  `dim` exists in the data. Should be set if `dim` might not exist, or
  if `dim` might potentially exist in a different main dimension than
  the one anticipated).

- raw:

  if set to FALSE inputs will be corrected (e.g. dots replaced by the
  letter "p") if necessary. If TRUE data will be written as is (risking
  the creation of inconsistent objects).

- value:

  a vector with the length of the main dimension the dimnames should be
  replaced in / added to. If set to NULL the corresponding dimension
  will be removed.

## Value

items of the requested dimension in the MAgPIE-object. If split=TRUE and
applied to a main dimension (1,2,3) a list of items for each
sub-dimension.

## Functions

- `getItems(x, dim, full = NULL, maindim = NULL, raw = FALSE) <- value`:
  set dimension names

## See also

[`dimCode`](dimCode.md)

Other GetterSetter: [`getCPR()`](getCPR.md),
[`getCells()`](getCells.md), [`getComment()`](getComment.md),
[`getCoords()`](getCoords.md), [`getDim()`](getDim.md),
[`getNames()`](getNames.md), [`getRegionList()`](getRegionList.md),
[`getRegions()`](getRegions.md), [`getSets()`](getSets.md),
[`getYears()`](getYears.md), [`setItems()`](setItems.md),
[`setNames-methods`](setNames-methods.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
x <- maxample("pop")
getItems(x, "scenario")
#> [1] "A2" "B1"
getItems(x, 3.1)
#> [1] "A2" "B1"
getItems(x, "i") <- paste0("REG", seq_len(ncells(x)))
getItems(x, "i")
#>  [1] "REG1"  "REG2"  "REG3"  "REG4"  "REG5"  "REG6"  "REG7"  "REG8" 
#>  [9] "REG9"  "REG10"
y <- x[, 1, ]
getItems(y, "t") <- NULL
```
