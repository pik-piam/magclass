# mplotMap

Render a simple world map of a coordinate-based (cell) magpie object.
Each grid cell is drawn as a colored tile on top of country outlines.
Country outlines require the suggested package `maps`; if it is not
installed, only the cell tiles are drawn and a message is emitted. If
the object contains more than one year or data name, the remaining
dimensions are spread across facets, so it is usually best to subset the
object to the slice(s) of interest before plotting.

## Usage

``` r
mplotMap(px, draw = TRUE)
```

## Arguments

- px:

  The magpie object to be visualized. It must contain spatial
  coordinates (see [`hasCoords`](hasCoords.md)).

- draw:

  Logical. If `TRUE` (the default), the plot is rendered on the current
  graphics device. If `FALSE`, the ggplot object is returned without
  drawing, so it can be modified or printed manually later.

## Value

Invisibly returns the ggplot object.

## See also

[`mplot`](mplot.md), [`hasCoords`](hasCoords.md)

Other Display: [`head.magpie()`](head.magpie.md),
[`maxample()`](maxample.md), [`mplot()`](mplot.md),
[`print.magpie()`](print.magpie.md), [`show-methods`](show-methods.md),
[`str.magpie()`](str.magpie.md)

## Author

Patrick Rein

## Examples

``` r
if (FALSE) { # \dontrun{
a <- maxample("animal")
mplotMap(a[, 1, 1])
} # }
```
