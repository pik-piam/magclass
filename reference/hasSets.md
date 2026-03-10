# Has Sets

Checks, whether set names have been set

## Usage

``` r
hasSets(x)
```

## Arguments

- x:

  MAgPIE object

## Value

Boolean indicating whether coordinates were found or not

## See also

[`getCoords`](getCoords.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
hasSets(maxample("pop"))
#> [1] TRUE
hasSets(maxample("animal"))
#> [1] TRUE
```
