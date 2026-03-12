# joins a data.frame or vector of strings with variable and unit separated into a data.frame with variable and unit joined as 'variable (unit)'. Use magclass::unitsplit to split them again

joins a data.frame or vector of strings with variable and unit separated
into a data.frame with variable and unit joined as 'variable (unit)'.
Use magclass::unitsplit to split them again

## Usage

``` r
unitjoin(x, unit = NULL, col = "variable")
```

## Arguments

- x:

  data.frame or vector of strings

- unit:

  vector of strings. If NULL, col 'unit' in x is used

- col:

  column name. Default: variable

## Value

data.frame or vector of strings, dependent on x

## See also

Other Utility: [`replace_non_finite()`](replace_non_finite.md),
[`unitsplit()`](unitsplit.md)
