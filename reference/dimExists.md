# dimExists

This function checks whether a dimension given by name or number exists.

## Usage

``` r
dimExists(dim, x, sep = ".")
```

## Arguments

- dim:

  A vector of dimension numbers or dimension names which should be
  checked for.

- x:

  MAgPIE object in which the dimensions should be searched for.

- sep:

  A character separating joined dimension names.

## Value

Boolean indicating whether the dimension exists or not.

## See also

[`dimCode`](dimCode.md)

Other ObjectInfo: [`fulldim()`](fulldim.md),
[`hasCoords()`](hasCoords.md), [`hasSets()`](hasSets.md),
[`is.temporal()`](is.temporal.md), [`isYear()`](isYear.md),
[`ncells()`](ncells.md), [`ndim()`](ndim.md),
[`sameDims()`](sameDims.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
dimExists(c("t", "scenario", "blablub"), pop)
#>        t scenario  blablub 
#>     TRUE     TRUE    FALSE 
```
