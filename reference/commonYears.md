# commonYears

Given (a list of) magclass objects, return the years (getYears) they
have in common, ignoring magclass objects with 0 or 1 years.

## Usage

``` r
commonYears(..., asInteger = FALSE)
```

## Arguments

- ...:

  One or more magclass objects, or alternatively a list of magclass
  objects.

- asInteger:

  Passed on to getYears. If TRUE the output format is 1995 otherwise
  "y1995".

## Value

The years (as returned by getYears) the input objects have in common.

## See also

Other TemporalOperations: [`convergence()`](convergence.md),
[`lowpass()`](lowpass.md), [`time_interpolate()`](time_interpolate.md)

## Author

Pascal Sauer
