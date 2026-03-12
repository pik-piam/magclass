# guessResolution

Guess the resolution of the given magpie object/coordinates by looking
at the minimum difference between unique sorted values. Fall back to 0.5
if guess is infinite.

## Usage

``` r
guessResolution(x)
```

## Arguments

- x:

  A magpie object or the coordinates of a magpie object (the result of
  [`getCoords`](getCoords.md))

## Value

The guessed resolution of the data

## See also

Other SpatialOperations:
[`spatRasterToDataset()`](spatRasterToDataset.md)

## Author

Jan Philipp Dietrich, Pascal Sauer
