# suppressSpecificWarnings

Like [`suppressWarnings`](https://rdrr.io/r/base/warning.html), but
instead of suppressing all warnings this only suppresses warnings if
they match the given pattern.

## Usage

``` r
suppressSpecificWarnings(expr, regularExpr, fixed = FALSE)
```

## Arguments

- expr:

  The expression/code to evaluate, can be a block of code inside curly
  braces.

- regularExpr:

  Only warnings matching this regular expression are suppressed.

- fixed:

  Match the literal string given by regularExpr instead of interpreting
  it as a regular expression. Passed to
  [`grepl`](https://rdrr.io/r/base/grep.html).

## Value

The result of evaluating expr.

## See also

[`suppressWarnings`](https://rdrr.io/r/base/warning.html)

## Author

Pascal Sauer
