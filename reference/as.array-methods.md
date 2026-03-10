# \~~ Methods for Function as.array \~~

\~~ Methods for function `as.array` \~~

## Usage

``` r
# S4 method for class 'magpie'
as.array(x)
```

## Arguments

- x:

  object which should be converted to an array

## Methods

- list("signature(x = \\ANY\\)"):

  standard as.array-method

- list("signature(x = \\magpie\\)"):

  Conversion takes place just by removing MAgPIE-object specific
  elements
