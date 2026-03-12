# Copy MAgPIE-files

This function copies MAgPIE-files from one location to another. During
the copying it is also possible to change the file type (e.g. from 'mz'
to 'csv')

## Usage

``` r
copy.magpie(input_file, output_file, round = NULL)
```

## Arguments

- input_file:

  file, that should be copied

- output_file:

  copy destination

- round:

  number of digits the values should be rounded. NULL means no rounding

## See also

[`read.magpie`](read.magpie.md),[`write.magpie`](write.magpie.md)

Other FileIO: [`read.magpie()`](read.magpie.md),
[`read.report()`](read.report.md), [`write.magpie()`](write.magpie.md),
[`write.report()`](write.report.md),
[`write.report2()`](write.report2.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
# copy.magpie("bla.csv","blub.mz")
```
