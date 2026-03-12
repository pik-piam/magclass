# Read MAgPIE-object from file

Reads a MAgPIE-file and converts it to a 3D array of the structure
(cells,years,datacolumn)

## Usage

``` r
read.magpie(
  file_name,
  file_folder = "",
  file_type = NULL,
  as.array = FALSE,
  comment.char = "*",
  check.names = FALSE,
  ...
)
```

## Arguments

- file_name:

  file name including file ending (wildcards are supported). Optionally
  also the full path can be specified here (instead of splitting it to
  file_name and file_folder)

- file_folder:

  folder the file is located in (alternatively you can also specify the
  full path in file_name - wildcards are supported)

- file_type:

  format the data is stored in. If file_type=NULL the file ending of the
  file_name is used as format. If format is different to the formats
  mentioned standard MAgPIE format is assumed. See
  [`write.magpie`](write.magpie.md) for a list of supported file
  formats.

- as.array:

  Should the input be transformed to an array? This can be useful for
  regional or global inputs, but all advantages of the magpie-class are
  lost.

- comment.char:

  character: a character vector of length one containing a single
  character or an empty string. Use "" to turn off the interpretation of
  comments altogether. If a comment is found it will be stored in
  attr(,"comment"). In text files the comment has to be at the beginning
  of the file in order to be recognized by read.magpie.

- check.names:

  logical. If TRUE then the names of the variables in the data frame are
  checked to ensure that they are syntactically valid variable names.
  Same functionality as in read.table.

- ...:

  additional arguments passed to specific read functions (e.g. `varname`
  for specifying the variable to be read in from a multi-variable NCDF
  file.)

## Value

- x:

  MAgPIE-object

## Details

See [`write.magpie`](write.magpie.md) for a list of supported file
formats.

## Note

See [`write.magpie`](write.magpie.md) for the detailed structure of
binary MAgPIE formats .m and .mz.

## See also

`"`[`magpie`](magpie-class.md)`"`, [`write.magpie`](write.magpie.md)

Other FileIO: [`copy.magpie()`](copy.magpie.md),
[`read.report()`](read.report.md), [`write.magpie()`](write.magpie.md),
[`write.report()`](write.report.md),
[`write.report2()`](write.report2.md)

## Author

Jan Philipp Dietrich, Stephen Bi, Florian Humpenoeder, Pascal Sauer
