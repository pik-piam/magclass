# Class "magpie" \~\~~

The MAgPIE class is a data format for cellular MAgPIE data with a close
relationship to the array data format. `is.magpie` tests if `x` is an
MAgPIE-object, `as.magpie` transforms `x` to an MAgPIE-object (if
possible).

## Arguments

- x:

  An object that should be either tested or transformed as/to an
  MAgPIE-object.

- ...:

  additional arguments supplied for the conversion to a MAgPIE object.
  Allowed arguments for arrays and dataframes are `spatial` and
  `temporal` both expecting a vector of dimension or column numbers
  which contain the spatial or temporal information. By default both
  arguments are set to NULL which means that the `as.magpie` will try to
  detect automatically the temporal and spatial dimensions. The
  arguments will just overwrite the automatic detection. If you want to
  specify that the data does not contain a spatial or temporal dimension
  you can set the corresponding argument to 0. In addition `as.magpie`
  for data.frames is also expecting an argument called `datacol` which
  expects a number stating which is the first column containing data.
  This argument should be used if the dimensions are not detected
  corretly, e.g. if the last dimension column contains years which are
  then detected as values and therefore interpreted as first data
  column. In addition an argument `tidy=TRUE` can be used to indicate
  that the data.frame structure is following the rules of tidy data
  (last column is the data column all other columns contain dimension
  information). This information will help the conversion. `sep` defines
  the dimension separator (default is ".") and `replacement` defines how
  the separator as a reserved character should be converted in order to
  not mess up with the object (default "\_"). Another available argument
  for conversions of data.frames and quitte objects to magpie is
  `filter` if set to TRUE (default) "." (separator) will be replaced
  withe the `replacement` character and empty entries will be replaced
  with a single space. If set to FALSE no filter will be applied to the
  data.

## Objects from the Class

Objects can be created by calls of the form
`new("magpie", data, dim, dimnames, ...)`. MAgPIE objects have three
dimensions (cells,years,datatype) and the dimensionnames of the first
dimension have the structure "REGION.cellnumber". MAgPIE-objects behave
the same like array-objects with 2 exceptions:  
1.Dimensions of the object will not collapse (e.g. `x[1,1,1]` will
remain 3D instead of becoming 1D)  
2.It is possible to extract full regions just by typing
`x["REGIONNAME",,]`.  
  

Please mind following standards:  
Header must not contain any purely numeric entries, but combinations of
characters and numbers are allowed (e.g. "bla","12" is forbidden, wheras
"bla","b12" is allowed)  
Years always have the structure "y" + 4-digit number, e.g. "y1995"  
Regions always have the structure 3 capital letters, e.g. "AFR" or
"GLO"  
  
This standards are necessary to allow the scripts to detect headers,
years and regions properly and to have a distinction to other data.

## See also

[`read.magpie`](read.magpie.md), [`write.magpie`](write.magpie.md),
[`getRegions`](getRegions.md), [`getYears`](getYears.md),
[`getNames`](getNames.md), [`getCPR`](getCPR.md), [`ncells`](ncells.md),
[`nyears`](ncells.md), [`ndata`](ncells.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
showClass("magpie")
#> Class "magpie" [package "magclass"]
#> 
#> Slots:
#>             
#> Name:  .Data
#> Class: array
#> 
#> Extends: 
#> Class "array", from data part
#> Class "structure", by class "array", distance 2
#> Class "vector", by class "array", distance 3, with explicit coerce

pop <- maxample("pop")

# returning PAO and PAS for 2025
pop["PA", 2025, , pmatch = "left"]
#>      scenario
#> i         A2     B1
#>   PAO 157.35 159.07
#>   PAS 668.49 639.68

# returning CPA for 2025
pop["PA", 2025, , pmatch = "right"]
#>      A2      B1 
#> 1592.09 1531.12 

# returning CPA PAO and PAS for 2025
pop["PA", 2025, , pmatch = TRUE]
#>      scenario
#> i          A2      B1
#>   CPA 1592.09 1531.12
#>   PAO  157.35  159.07
#>   PAS  668.49  639.68

# returning PAS and 2025
pop["PAS", 2025, ]
#>     A2     B1 
#> 668.49 639.68 

# return all entries for year 2025
pop[2025, dim = 2]
#>      scenario
#> i          A2      B1
#>   AFR 1124.11 1118.33
#>   CPA 1592.09 1531.12
#>   EUR  605.27  613.98
#>   FSU  308.59  309.78
#>   LAM  733.13  681.60
#>   MEA  596.13  598.73
#>   NAM  382.53  376.11
#>   PAO  157.35  159.07
#>   PAS  668.49  639.68
#>   SAS 2095.48 1870.96

# returning everything but values for PAS or values for 2025
pop["PAS", 2025, , invert = TRUE]
#> , , scenario = A2
#> 
#>      t
#> i         y1995   y2005   y2015   y2035   y2045   y2055   y2065
#>   AFR  552.6664  696.44  889.18 1389.33 1659.73 1924.19 2172.30
#>   CPA 1280.6350 1429.53 1518.46 1640.95 1671.94 1691.24 1719.25
#>   EUR  554.4384  582.36  593.76  614.58  618.97  619.37  618.74
#>   FSU  276.3431  295.38  302.62  313.30  315.72  317.36  319.61
#>   LAM  451.9981  558.29  646.02  812.69  880.98  939.44  989.54
#>   MEA  277.7437  390.18  489.22  698.33  790.61  871.83  942.81
#>   NAM  292.1132  326.09  353.25  409.44  431.12  448.70  465.22
#>   PAO  133.7772  152.00  155.27  158.81  159.70  160.45  160.95
#>   SAS 1269.9243 1505.02 1796.76 2369.60 2600.68 2783.75 2920.70
#>      t
#> i       y2075   y2085   y2095   y2105   y2115   y2125   y2135   y2145
#>   AFR 2387.96 2560.32 2671.07 2708.86 2708.86 2708.86 2708.86 2708.86
#>   CPA 1765.77 1832.31 1918.47 1965.05 1965.05 1965.05 1965.05 1965.05
#>   EUR  622.03  630.52  642.20  648.98  648.98  648.98  648.98  648.98
#>   FSU  322.30  327.08  332.39  334.66  334.66  334.66  334.66  334.66
#>   LAM 1035.25 1079.39 1117.61 1134.64 1134.64 1134.64 1134.64 1134.64
#>   MEA 1002.56 1052.19 1088.94 1103.31 1103.31 1103.31 1103.31 1103.31
#>   NAM  481.39  494.88  505.93  511.41  511.41  511.41  511.41  511.41
#>   PAO  161.47  163.53  166.31  167.49  167.49  167.49  167.49  167.49
#>   SAS 3006.60 3040.10 3007.86 2972.39 2972.39 2972.39 2972.39 2972.39
#> 
#> , , scenario = B1
#> 
#>      t
#> i         y1995   y2005   y2015   y2035   y2045   y2055   y2065
#>   AFR  552.6664  721.85  932.04 1267.33 1383.24 1469.16 1510.27
#>   CPA 1280.6350 1429.26 1499.74 1518.73 1463.68 1370.97 1257.23
#>   EUR  554.4384  587.21  603.63  619.48  617.12  606.77  592.52
#>   FSU  276.3431  296.84  305.26  311.47  309.03  301.99  292.46
#>   LAM  451.9981  552.79  623.20  723.44  747.70  753.98  743.05
#>   MEA  277.7437  398.92  502.51  682.80  754.14  811.59  849.11
#>   NAM  292.1132  325.04  349.85  399.68  418.70  434.27  449.98
#>   PAO  133.7772  153.07  157.37  159.51  158.10  155.21  151.86
#>   SAS 1269.9243 1475.64 1687.80 1999.15 2072.68 2090.96 2049.18
#>      t
#> i       y2075   y2085   y2095   y2105   y2115   y2125   y2135   y2145
#>   AFR 1505.16 1454.54 1361.24 1304.59 1304.59 1304.59 1304.59 1304.59
#>   CPA 1139.25 1021.52  904.61  846.50  846.50  846.50  846.50  846.50
#>   EUR  579.18  567.73  554.61  547.06  547.06  547.06  547.06  547.06
#>   FSU  281.39  269.77  257.52  251.04  251.04  251.04  251.04  251.04
#>   LAM  718.79  683.68  637.69  611.88  611.88  611.88  611.88  611.88
#>   MEA  865.89  861.01  831.23  809.60  809.60  809.60  809.60  809.60
#>   NAM  468.05  486.99  503.86  511.44  511.44  511.44  511.44  511.44
#>   PAO  148.08  144.47  140.82  138.80  138.80  138.80  138.80  138.80
#>   SAS 1953.77 1811.83 1629.07 1528.15 1528.15 1528.15 1528.15 1528.15
#> 

# accessing subdimension via set name

a <- maxample("animal")
a[list(country = "NLD", y = "53p25"), , list(species = c("rabbit", "dog"))]
#> , , type.species.color = animal.rabbit.black
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8           10            13
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             17           23            31
#>                       year.month.day
#> x.y.country.cell       y2002.april.20 y2002.may.20 y2002.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             44           64            97
#>                       year.month.day
#> x.y.country.cell       y2002.august.20
#>   5p75.53p25.NLD.14084               0
#>   6p25.53p25.NLD.14113               0
#>   6p75.53p25.NLD.14141             153
#> 
#> , , type.species.color = animal.rabbit.white
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              6            7             9
#>   6p25.53p25.NLD.14113              4            5             6
#>   6p75.53p25.NLD.14141              4            5             6
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084             11           14            18
#>   6p25.53p25.NLD.14113              7            9            11
#>   6p75.53p25.NLD.14141              7            9            11
#>                       year.month.day
#> x.y.country.cell       y2002.april.20 y2002.may.20 y2002.june.20
#>   5p75.53p25.NLD.14084             24           33            47
#>   6p25.53p25.NLD.14113             14           18            24
#>   6p75.53p25.NLD.14141             14           18            24
#>                       year.month.day
#> x.y.country.cell       y2002.august.20
#>   5p75.53p25.NLD.14084              69
#>   6p25.53p25.NLD.14113              33
#>   6p75.53p25.NLD.14141              33
#> 
#> , , type.species.color = animal.dog.brown
#> 
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8            8             8
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8            8             8
#>                       year.month.day
#> x.y.country.cell       y2002.april.20 y2002.may.20 y2002.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8            8             8
#>                       year.month.day
#> x.y.country.cell       y2002.august.20
#>   5p75.53p25.NLD.14084               0
#>   6p25.53p25.NLD.14113               0
#>   6p75.53p25.NLD.14141               8
#> 

# please note that the list elements act as filter. For instance, the
# following example will not contain any dogs as the data set does
# not contain any dogs which are black.
a[list(country = "NLD", y = "53p25"), , list(species = c("rabbit", "dog"), color = "black")]
#>                       year.month.day
#> x.y.country.cell       y2000.april.20 y2000.may.20 y2000.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141              8           10            13
#>                       year.month.day
#> x.y.country.cell       y2001.april.20 y2001.may.20 y2001.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             17           23            31
#>                       year.month.day
#> x.y.country.cell       y2002.april.20 y2002.may.20 y2002.june.20
#>   5p75.53p25.NLD.14084              0            0             0
#>   6p25.53p25.NLD.14113              0            0             0
#>   6p75.53p25.NLD.14141             44           64            97
#>                       year.month.day
#> x.y.country.cell       y2002.august.20
#>   5p75.53p25.NLD.14084               0
#>   6p25.53p25.NLD.14113               0
#>   6p75.53p25.NLD.14141             153

# it is also possible to extract given combinations of subdimensions
# via a data-frame
df <- data.frame(getItems(a, 3, split = TRUE, full = TRUE))[c(1, 3, 4), ][3:2]
getItems(a[df], 3)
#> [1] "animal.rabbit.black" "animal.bird.black"   "animal.bird.red"    

# Unknown dimensions to be added in output!
df$blub <- paste0("bl", 1:dim(df)[1])
getItems(a[df], 3)
#> [1] "animal.rabbit.black.bl1" "animal.bird.black.bl2"  
#> [3] "animal.bird.red.bl3"    
```
