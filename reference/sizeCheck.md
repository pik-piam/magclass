# sizeCheck

Calculates expected magclass object length and checks that it stays
below the limit defined with magclass_sizeLimit (default = 10^9). This
is useful to prevent out of memory errors in case of unwanted object
expansions Ignored if `getOption("magclass_sizeLimit")` is negative.

## Usage

``` r
sizeCheck(dim)
```

## Arguments

- dim:

  dimensions of the current object as returned by function `dim`

## Author

Jan Philipp Dietrich

## Examples

``` r
pop <- maxample("pop")
magclass:::sizeCheck(dim(pop))

if (FALSE) { # \dontrun{
magclass:::sizeCheck(c(6765L, 10946L, 17711L))
} # }
```
