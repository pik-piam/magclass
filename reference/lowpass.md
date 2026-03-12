# Lowpass Filter

Filters high frequencies out of a time series. The filter has the
structure \`x'(n) = (x(n-1)+2\*x(n)+x(n+1))/4\`

## Usage

``` r
lowpass(x, i = 1, fix = NULL, altFilter = NULL, warn = TRUE)
```

## Arguments

- x:

  Vector of data points, that should be filtered or MAgPIE object

- i:

  number of iterations the filter should be applied to the data

- fix:

  Fixes the starting and/or ending data point. Default value is `NULL`
  which doesn't fix any point. Available options are: `"start"` for
  fixing the starting point, `"end"` for fixing the ending point and
  `"both"` for fixing both ends of the data.

- altFilter:

  set special filter rule to indexes defined in this parameter. Special
  filter structure: \`x'(n) = (2\*x(n)+x(n+1))/3\`

- warn:

  boolean deciding whether lowpass issues a warning for critical
  parameter choices or not

## Value

The filtered data vector or MAgPIE object

## See also

Other TemporalOperations: [`commonYears()`](commonYears.md),
[`convergence()`](convergence.md),
[`time_interpolate()`](time_interpolate.md)

## Author

Jan Philipp Dietrich, Misko Stevanovic

## Examples

``` r
lowpass(c(1, 2, 11, 3, 4))
#> [1] 1.25 4.00 6.75 5.25 3.75
# to fix the starting point
lowpass(c(0, 9, 1, 5, 14, 20, 6, 11, 0), i = 2, fix = "start")
#> Warning: Fixing start or end does modify the total sum of values! Use fix=NULL to let the total sum unchanged!
#> [1]  0.0000  3.3750  4.7500  7.4375 11.9375 13.5000 10.8750  6.8750
#> [9]  3.8125
```
