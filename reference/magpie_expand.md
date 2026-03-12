# magpie_expand

Expands a MAgPIE object based on a reference

## Usage

``` r
magpie_expand(x, ref)
```

## Arguments

- x:

  MAgPIE object that should be expanded

- ref:

  MAgPIE object that serves as a reference

## Value

An expanded version of x.

## Details

Expansion means here that the dimensions of x are expanded accordingly
to ref. Please note that this is really only about expansion. In the
case that one dimension of ref is smaller than of x nothing happens with
this dimension.

You can influence the verbosity of this function by setting the option
"magclass.verbosity". By default verbosity is set to 1 which means that
only warnings are returned. Setting verbosity to 2 means that warnings
as well as additional notes are returned. This is done by
options(verbosity.level=2)

With version 5 of the package magpie_expand has been updated to a newer
version (currently 2.1) and since version 6 this is the only currently
supported version. To switch to the old setup you have to install
magclass in a version \< 6 and set `options(magclass_expand_version=1)`.

By default expansion is based on the elements in a dimension ignoring
the set name of the dimension. To expand based on set names instead of
contents (recommended) you can switch
`options(magclass_setMatching=TRUE)`. Please be careful with this
setting as it alters the behavior of magclass objects quite
significantly! For more information have a look at
[`vignette("magclass-expansion")`](../articles/magclass-expansion.md).

## See also

[`as.magpie`](magpie-class.md),
[`options`](https://rdrr.io/r/base/options.html)

Other DataBinding: [`cbind.magpie()`](cbind.magpie.md),
[`extend()`](extend.md), [`magpiesort()`](magpiesort.md),
[`matchDim()`](matchDim.md), [`mbind()`](mbind.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
a <- new.magpie(c("AFR", "CPA"), "y1995", c("m", "n"))
b <- new.magpie("GLO", "y1995", c("bla", "blub"))
magpie_expand(b, a)
#>       data.data1
#> region m.bla m.blub n.bla n.blub
#>    AFR    NA     NA    NA     NA
#>    CPA    NA     NA    NA     NA
options(magclass.verbosity = 2)
magpie_expand(b, a)
#>       data.data1
#> region m.bla m.blub n.bla n.blub
#>    AFR    NA     NA    NA     NA
#>    CPA    NA     NA    NA     NA
```
