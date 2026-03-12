# Write file in report format

This function writes the content of a MAgPIE object into a file or
returns it directly using the reporting format as it is used for many
model inter-comparisons.

## Usage

``` r
write.report(
  x,
  file = NULL,
  model = NULL,
  scenario = NULL,
  unit = NULL,
  ndigit = 4,
  append = FALSE,
  skipempty = TRUE,
  extracols = NULL
)
```

## Arguments

- x:

  MAgPIE object or a list of lists with MAgPIE objects as created by
  read.report. In the latter case settings for model and scenario are
  overwritten by the information given in the list.

- file:

  file name the object should be written to. If NULL the formatted
  content is returned

- model:

  Name of the model which calculated the results

- scenario:

  The scenario which was used to get that results.

- unit:

  Unit of the data. Only relevant if unit is not already supplied in
  Dimnames (format "name (unit)"). Can be either a single string or a
  vector of strings with a length equal to the number of different data
  elements in the MAgPIE object

- ndigit:

  Number of digits the output should have

- append:

  Logical which decides whether data should be added to an existing file
  or an existing file should be overwritten

- skipempty:

  Determines whether empty entries (all data NA) should be written to
  file or not.

- extracols:

  names of dimensions which should appear in the output as additional
  columns

## See also

[`read.report`](read.report.md)

Other FileIO: [`copy.magpie()`](copy.magpie.md),
[`read.magpie()`](read.magpie.md), [`read.report()`](read.report.md),
[`write.magpie()`](write.magpie.md),
[`write.report2()`](write.report2.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
write.report(maxample("pop"))
#>    Model Scenario Region Variable Unit      1995    2005    2015
#> 1    N/A       A2    AFR      N/A  N/A  552.6664  696.44  889.18
#> 3    N/A       A2    CPA      N/A  N/A 1280.6350 1429.53 1518.46
#> 5    N/A       A2    EUR      N/A  N/A  554.4384  582.36  593.76
#> 7    N/A       A2    FSU      N/A  N/A  276.3431  295.38  302.62
#> 9    N/A       A2    LAM      N/A  N/A  451.9981  558.29  646.02
#> 11   N/A       A2    MEA      N/A  N/A  277.7437  390.18  489.22
#> 13   N/A       A2    NAM      N/A  N/A  292.1132  326.09  353.25
#> 15   N/A       A2    PAO      N/A  N/A  133.7772  152.00  155.27
#> 17   N/A       A2    PAS      N/A  N/A  383.2277  534.73  604.94
#> 19   N/A       A2    SAS      N/A  N/A 1269.9243 1505.02 1796.76
#> 2    N/A       B1    AFR      N/A  N/A  552.6664  721.85  932.04
#> 4    N/A       B1    CPA      N/A  N/A 1280.6350 1429.26 1499.74
#> 6    N/A       B1    EUR      N/A  N/A  554.4384  587.21  603.63
#> 8    N/A       B1    FSU      N/A  N/A  276.3431  296.84  305.26
#> 10   N/A       B1    LAM      N/A  N/A  451.9981  552.79  623.20
#> 12   N/A       B1    MEA      N/A  N/A  277.7437  398.92  502.51
#> 14   N/A       B1    NAM      N/A  N/A  292.1132  325.04  349.85
#> 16   N/A       B1    PAO      N/A  N/A  133.7772  153.07  157.37
#> 18   N/A       B1    PAS      N/A  N/A  383.2277  530.67  590.42
#> 20   N/A       B1    SAS      N/A  N/A 1269.9243 1475.64 1687.80
#>       2025    2035    2045    2055    2065    2075    2085    2095
#> 1  1124.11 1389.33 1659.73 1924.19 2172.30 2387.96 2560.32 2671.07
#> 3  1592.09 1640.95 1671.94 1691.24 1719.25 1765.77 1832.31 1918.47
#> 5   605.27  614.58  618.97  619.37  618.74  622.03  630.52  642.20
#> 7   308.59  313.30  315.72  317.36  319.61  322.30  327.08  332.39
#> 9   733.13  812.69  880.98  939.44  989.54 1035.25 1079.39 1117.61
#> 11  596.13  698.33  790.61  871.83  942.81 1002.56 1052.19 1088.94
#> 13  382.53  409.44  431.12  448.70  465.22  481.39  494.88  505.93
#> 15  157.35  158.81  159.70  160.45  160.95  161.47  163.53  166.31
#> 17  668.49  723.13  767.30  798.68  819.21  834.31  844.38  843.52
#> 19 2095.48 2369.60 2600.68 2783.75 2920.70 3006.60 3040.10 3007.86
#> 2  1118.33 1267.33 1383.24 1469.16 1510.27 1505.16 1454.54 1361.24
#> 4  1531.12 1518.73 1463.68 1370.97 1257.23 1139.25 1021.52  904.61
#> 6   613.98  619.48  617.12  606.77  592.52  579.18  567.73  554.61
#> 8   309.78  311.47  309.03  301.99  292.46  281.39  269.77  257.52
#> 10  681.60  723.44  747.70  753.98  743.05  718.79  683.68  637.69
#> 12  598.73  682.80  754.14  811.59  849.11  865.89  861.01  831.23
#> 14  376.11  399.68  418.70  434.27  449.98  468.05  486.99  503.86
#> 16  159.07  159.51  158.10  155.21  151.86  148.08  144.47  140.82
#> 18  639.68  674.98  692.45  689.79  668.98  634.64  590.05  536.24
#> 20 1870.96 1999.15 2072.68 2090.96 2049.18 1953.77 1811.83 1629.07
#>       2105    2115    2125    2135    2145
#> 1  2708.86 2708.86 2708.86 2708.86 2708.86
#> 3  1965.05 1965.05 1965.05 1965.05 1965.05
#> 5   648.98  648.98  648.98  648.98  648.98
#> 7   334.66  334.66  334.66  334.66  334.66
#> 9  1134.64 1134.64 1134.64 1134.64 1134.64
#> 11 1103.31 1103.31 1103.31 1103.31 1103.31
#> 13  511.41  511.41  511.41  511.41  511.41
#> 15  167.49  167.49  167.49  167.49  167.49
#> 17  839.53  839.53  839.53  839.53  839.53
#> 19 2972.39 2972.39 2972.39 2972.39 2972.39
#> 2  1304.59 1304.59 1304.59 1304.59 1304.59
#> 4   846.50  846.50  846.50  846.50  846.50
#> 6   547.06  547.06  547.06  547.06  547.06
#> 8   251.04  251.04  251.04  251.04  251.04
#> 10  611.88  611.88  611.88  611.88  611.88
#> 12  809.60  809.60  809.60  809.60  809.60
#> 14  511.44  511.44  511.44  511.44  511.44
#> 16  138.80  138.80  138.80  138.80  138.80
#> 18  507.06  507.06  507.06  507.06  507.06
#> 20 1528.15 1528.15 1528.15 1528.15 1528.15
```
