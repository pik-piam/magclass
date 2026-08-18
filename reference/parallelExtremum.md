# parallelExtremum

(for internal use) Applies base::pmin/base::pmax to the underlying
arrays of magpie objects whose dimensions have already been aligned by
[`withAlignedDims`](withAlignedDims.md).

## Usage

``` r
parallelExtremum(func, na.rm = FALSE, ...)
```

## Arguments

- func:

  base::pmin or base::pmax

- na.rm:

  Passed on to func

- ...:

  Magpie objects with identical dimensions, as returned by
  withAlignedDims

## Details

base::pmin/base::pmax compare and subassign their arguments directly.
Handing them magpie objects makes every one of those steps dispatch to
the magclass Ops and \`\[\<-\` methods, which realign the operands by
name again for each comparison. Reducing the plain arrays instead avoids
that, and is roughly 80 times faster on large objects.

## Author

Patrick Rein
