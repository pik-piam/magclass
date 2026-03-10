# pmin/pmax

magclass-aware pmin/pmax, which calculate the parallel minima/maxima of
the input values

## Usage

``` r
# S4 method for class 'magpie'
pmax(..., na.rm = FALSE)

# S4 method for class 'magpie'
pmin(..., na.rm = FALSE)
```

## Arguments

- ...:

  Multiple magpie objects with dimensions that each are either the same
  or only contain one or no item among all objects.

- na.rm:

  Passed to [`pmin`](https://rdrr.io/r/base/Extremes.html)

## Value

A new magpie object that contains the minimum/maximum of values between
all the passed magpie objects.

## Details

[`pmin`](https://rdrr.io/r/base/Extremes.html)/[`pmax`](https://rdrr.io/r/base/Extremes.html)
do not care about the order of items in a magclass object's dim.
magclass pmin/pmax reorder items in each dim so they are in the same
order.

## Author

Pascal Sauer, Patrick Rein
