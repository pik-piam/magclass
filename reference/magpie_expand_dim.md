# magpie_expand_dim

Expands a single MAgPIE object dimension

## Usage

``` r
magpie_expand_dim(x, ref, dim = 1)
```

## Arguments

- x:

  MAgPIE object that should be expanded

- ref:

  MAgPIE object that serves as a reference

- dim:

  dimension that should be expanded

## Value

An expanded version of x.

## Details

Expansion means here that the dimensions of x are expanded acordingly to
ref. Please note that this is really only about expansion. In the case
that one dimension of ref is smaller than of x nothing happens with this
dimension. At the moment magpie_expand is only internally available in
the magclass library

In contrast to [`magpie_expand`](magpie_expand.md) this function is
expanding only a single dimension. It is meant as a support function for
[`magpie_expand`](magpie_expand.md) itself.

## See also

[`as.magpie`](magpie-class.md),
[`options`](https://rdrr.io/r/base/options.html)

## Author

Jan Philipp Dietrich

## Examples

``` r
d <- new.magpie(c("AFR.BLUB.1", "AFR.BLUB.2", "EUR.BLUB.1",
                    "AFR.BLA.1", "AFR.BLA.2", "EUR.BLA.1"), fill = 1)
 getSets(d)[1:3] <- c("reg", "b", "i")
 e <- new.magpie(c("BLA.AFR.A", "BLA.EUR.A", "BLUB.AFR.A", "BLUB.EUR.A",
                    "BLA.AFR.B", "BLA.EUR.B", "BLUB.AFR.B", "BLUB.EUR.B"), fill = 2)
 getSets(e)[1:3] <- c("b", "reg", "a")
 magclass:::magpie_expand_dim(d, e, dim = 1)
#>  BLA.AFR.A.1  BLA.AFR.A.2  BLA.EUR.A.1 BLUB.AFR.A.1 BLUB.AFR.A.2 
#>            1            1            1            1            1 
#> BLUB.EUR.A.1  BLA.AFR.B.1  BLA.AFR.B.2  BLA.EUR.B.1 BLUB.AFR.B.1 
#>            1            1            1            1            1 
#> BLUB.AFR.B.2 BLUB.EUR.B.1 
#>            1            1 
```
