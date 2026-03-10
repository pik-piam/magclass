# Write a magpie object to a netCDF file

Write a magpie object to a netCDF file

## Usage

``` r
writeNC(
  x,
  filename,
  unit,
  ...,
  compression = 2,
  missval = NA,
  gridDefinition = NULL,
  zname = "time",
  progress = FALSE
)
```

## Arguments

- x:

  A magpie object

- filename:

  Name of the netCDF file to write

- unit:

  Unit of the data, to omit pass "" (empty string)

- ...:

  For future expansion

- compression:

  Level of compression to use (1-9), NA for no compression

- missval:

  The value that encodes NA in the resulting netCDF file

- gridDefinition:

  A vector of 5 numeric values: c(xMin, xMax, yMin, yMax, resolution).
  Use c(-179.75, 179.75, -89.75, 89.75, 0.5) to write a standard
  0.5-degree-resolution lon/lat grid. If NULL, use min/max of
  coordinates in x and guessResolution

- zname:

  Name of the z dimension in the netCDF file

- progress:

  If TRUE, print progress messages

## Author

Pascal Sauer
