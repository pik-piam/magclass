# \~~ Methods for Function colSums and colMeans \~~

\~~ Methods for function `colSums` and `colMeans` \~~

## Usage

``` r
# S4 method for class 'magpie'
colSums(x, na.rm = FALSE, dims = 1, ...)
```

## Arguments

- x:

  object on which calculation should be performed

- na.rm:

  logical. Should missing values (including NaN) be omitted from the
  calculations?

- dims:

  integer: Which dimensions are regarded as "rows" or "columns" to sum
  over. For row\*, the sum or mean is over dimensions dims+1, ...; for
  col\* it is over dimensions 1:dims.

- ...:

  further arguments passed to other colSums/colMeans methods

## Methods

- list("signature(x = \\ANY\\)"):

  normal colSums and colMeans method

- list("signature(x = \\magpie\\)"):

  classical method prepared to handle MAgPIE objects

## See also

Other Aggregation: [`dimSums()`](dimSums.md),
[`rowSums-methods`](rowSums-methods.md)
