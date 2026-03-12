# new.magpie

Creates a new MAgPIE object

## Usage

``` r
new.magpie(
  cells_and_regions = "GLO",
  years = NULL,
  names = NULL,
  fill = NA,
  sort = FALSE,
  sets = NULL,
  unit = NULL
)
```

## Arguments

- cells_and_regions:

  Either the region names (e.g. "AFR"), or the cells (e.g. 1:10), or
  both in combination (e.g. "AFR.1"). NULL means no spatial element.

- years:

  dimnames for years in the format "yXXXX" or as integers. NULL means
  one year which is not further specified

- names:

  dimnames for names. NULL means one data element which is not further
  specified

- fill:

  Default value for the MAgPIE object

- sort:

  Bolean. Decides, wheher output should be sorted or not.

- sets:

  A vector of dimension names. See [`getSets`](getSets.md) for more
  information.

- unit:

  deprecated

## Value

an empty magpie object filled with fill, with the given dimnames

## See also

[`as.magpie`](magpie-class.md)

Other ObjectCreation: [`clean_magpie()`](clean_magpie.md),
[`complete_magpie()`](complete_magpie.md),
[`copy.attributes()`](copy.attributes.md)

## Author

Benjamin Bodirsky, Jan Philipp Dietrich

## Examples

``` r
a <- new.magpie(1:10, 1995:2000)
b <- new.magpie(c("AFR", "CPA"), "y1995", c("bla", "blub"), sets = c("i", "t", "value"))
c <- new.magpie()
```
