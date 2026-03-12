# magpie method for tibble::as_tibble

magpie method for tibble::as_tibble

## Usage

``` r
# S3 method for class 'magpie'
as_tibble(
  x,
  ...,
  .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL)
)
```

## Arguments

- x:

  A magpie object

- ...:

  Unused, for extensibility.

- .rows:

  The number of rows, useful to create a 0-column tibble or just as an
  additional check.

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence,

  - `"unique"`: Make sure names are unique and not empty,

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`,

  - `"universal"`: Make the names `unique` and syntactic

  - `"unique_quiet"`: Same as `"unique"`, but "quiet"

  - `"universal_quiet"`: Same as `"universal"`, but "quiet"

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base R).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

  This argument is passed on as `repair` to
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).
  See there for more details on these terms and the strategies used to
  enforce them.

- rownames:

  How to treat existing row names of a data frame or matrix:

  - `NULL`: remove row names. This is the default.

  - `NA`: keep row names.

  - A string: the name of a new column. Existing rownames are
    transferred into this column and the `row.names` attribute is
    deleted. No name repair is applied to the new column name, even if
    `x` already contains a column of that name. Use
    `as_tibble(rownames_to_column(...))` to safeguard against this case.

  Read more in
  [rownames](https://tibble.tidyverse.org/reference/rownames.html).

## Value

A tibble object

## See also

Other MAgPIE-Conversions: [`as.RasterBrick()`](as.RasterBrick.md),
[`as.SpatRaster()`](as.SpatRaster.md),
[`as.SpatRasterDataset()`](as.SpatRasterDataset.md),
[`as.SpatVector()`](as.SpatVector.md),
[`as.array-methods`](as.array-methods.md),
[`as.data.frame-methods`](as.data.frame-methods.md),
[`unwrap()`](unwrap.md), [`wrap()`](wrap.md)
