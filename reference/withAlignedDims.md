# withAlignedDims

(for internal use) Executes the passed function after reordering items
in each dimension for all passed magpie objects

## Usage

``` r
withAlignedDims(func, funcName = "anonymous func", ...)
```

## Arguments

- func:

  The function that should be executed with prior alignment of
  dimensions

- funcName:

  The name of the resulting function, to improve error messages

## Author

Pascal Sauer, Patrick Rein
