# where

Analysis function for magpie objects

## Usage

``` r
where(x, plot = NULL)
```

## Arguments

- x:

  A logical statement with a magpie object

- plot:

  deprecated. Use the function whereplot in package luplot.

## Value

A list of analysis parameters

## See also

whereplot in package luplot

Other SelectionCalculation: [`magpply()`](magpply.md),
[`mcalc()`](mcalc.md), [`mselect()`](mselect.md)

## Author

Benjamin Leon Bodirsky, Jan Philipp Dietrich

## Examples

``` r
p <- maxample("pop")
where(p > 500)
#> $true
#> $true$individual
#>     i     t       scenario
#> AFR "AFR" "y1995" "A2"    
#> CPA "CPA" "y1995" "A2"    
#> EUR "EUR" "y1995" "A2"    
#> SAS "SAS" "y1995" "A2"    
#> AFR "AFR" "y2005" "A2"    
#> CPA "CPA" "y2005" "A2"    
#> EUR "EUR" "y2005" "A2"    
#> LAM "LAM" "y2005" "A2"    
#> PAS "PAS" "y2005" "A2"    
#> SAS "SAS" "y2005" "A2"    
#> AFR "AFR" "y2015" "A2"    
#> CPA "CPA" "y2015" "A2"    
#> EUR "EUR" "y2015" "A2"    
#> LAM "LAM" "y2015" "A2"    
#> PAS "PAS" "y2015" "A2"    
#> SAS "SAS" "y2015" "A2"    
#> AFR "AFR" "y2025" "A2"    
#> CPA "CPA" "y2025" "A2"    
#> EUR "EUR" "y2025" "A2"    
#> LAM "LAM" "y2025" "A2"    
#> MEA "MEA" "y2025" "A2"    
#> PAS "PAS" "y2025" "A2"    
#> SAS "SAS" "y2025" "A2"    
#> AFR "AFR" "y2035" "A2"    
#> CPA "CPA" "y2035" "A2"    
#> EUR "EUR" "y2035" "A2"    
#> LAM "LAM" "y2035" "A2"    
#> MEA "MEA" "y2035" "A2"    
#> PAS "PAS" "y2035" "A2"    
#> SAS "SAS" "y2035" "A2"    
#> AFR "AFR" "y2045" "A2"    
#> CPA "CPA" "y2045" "A2"    
#> EUR "EUR" "y2045" "A2"    
#> LAM "LAM" "y2045" "A2"    
#> MEA "MEA" "y2045" "A2"    
#> PAS "PAS" "y2045" "A2"    
#> SAS "SAS" "y2045" "A2"    
#> AFR "AFR" "y2055" "A2"    
#> CPA "CPA" "y2055" "A2"    
#> EUR "EUR" "y2055" "A2"    
#> LAM "LAM" "y2055" "A2"    
#> MEA "MEA" "y2055" "A2"    
#> PAS "PAS" "y2055" "A2"    
#> SAS "SAS" "y2055" "A2"    
#> AFR "AFR" "y2065" "A2"    
#> CPA "CPA" "y2065" "A2"    
#> EUR "EUR" "y2065" "A2"    
#> LAM "LAM" "y2065" "A2"    
#> MEA "MEA" "y2065" "A2"    
#> PAS "PAS" "y2065" "A2"    
#> SAS "SAS" "y2065" "A2"    
#> AFR "AFR" "y2075" "A2"    
#> CPA "CPA" "y2075" "A2"    
#> EUR "EUR" "y2075" "A2"    
#> LAM "LAM" "y2075" "A2"    
#> MEA "MEA" "y2075" "A2"    
#> PAS "PAS" "y2075" "A2"    
#> SAS "SAS" "y2075" "A2"    
#> AFR "AFR" "y2085" "A2"    
#> CPA "CPA" "y2085" "A2"    
#> EUR "EUR" "y2085" "A2"    
#> LAM "LAM" "y2085" "A2"    
#> MEA "MEA" "y2085" "A2"    
#> PAS "PAS" "y2085" "A2"    
#> SAS "SAS" "y2085" "A2"    
#> AFR "AFR" "y2095" "A2"    
#> CPA "CPA" "y2095" "A2"    
#> EUR "EUR" "y2095" "A2"    
#> LAM "LAM" "y2095" "A2"    
#> MEA "MEA" "y2095" "A2"    
#> NAM "NAM" "y2095" "A2"    
#> PAS "PAS" "y2095" "A2"    
#> SAS "SAS" "y2095" "A2"    
#> AFR "AFR" "y2105" "A2"    
#> CPA "CPA" "y2105" "A2"    
#> EUR "EUR" "y2105" "A2"    
#> LAM "LAM" "y2105" "A2"    
#> MEA "MEA" "y2105" "A2"    
#> NAM "NAM" "y2105" "A2"    
#> PAS "PAS" "y2105" "A2"    
#> SAS "SAS" "y2105" "A2"    
#> AFR "AFR" "y2115" "A2"    
#> CPA "CPA" "y2115" "A2"    
#> EUR "EUR" "y2115" "A2"    
#> LAM "LAM" "y2115" "A2"    
#> MEA "MEA" "y2115" "A2"    
#> NAM "NAM" "y2115" "A2"    
#> PAS "PAS" "y2115" "A2"    
#> SAS "SAS" "y2115" "A2"    
#> AFR "AFR" "y2125" "A2"    
#> CPA "CPA" "y2125" "A2"    
#> EUR "EUR" "y2125" "A2"    
#> LAM "LAM" "y2125" "A2"    
#> MEA "MEA" "y2125" "A2"    
#> NAM "NAM" "y2125" "A2"    
#> PAS "PAS" "y2125" "A2"    
#> SAS "SAS" "y2125" "A2"    
#> AFR "AFR" "y2135" "A2"    
#> CPA "CPA" "y2135" "A2"    
#> EUR "EUR" "y2135" "A2"    
#> LAM "LAM" "y2135" "A2"    
#> MEA "MEA" "y2135" "A2"    
#> NAM "NAM" "y2135" "A2"    
#> PAS "PAS" "y2135" "A2"    
#> SAS "SAS" "y2135" "A2"    
#> AFR "AFR" "y2145" "A2"    
#> CPA "CPA" "y2145" "A2"    
#> EUR "EUR" "y2145" "A2"    
#> LAM "LAM" "y2145" "A2"    
#> MEA "MEA" "y2145" "A2"    
#> NAM "NAM" "y2145" "A2"    
#> PAS "PAS" "y2145" "A2"    
#> SAS "SAS" "y2145" "A2"    
#> AFR "AFR" "y1995" "B1"    
#> CPA "CPA" "y1995" "B1"    
#> EUR "EUR" "y1995" "B1"    
#> SAS "SAS" "y1995" "B1"    
#> AFR "AFR" "y2005" "B1"    
#> CPA "CPA" "y2005" "B1"    
#> EUR "EUR" "y2005" "B1"    
#> LAM "LAM" "y2005" "B1"    
#> PAS "PAS" "y2005" "B1"    
#> SAS "SAS" "y2005" "B1"    
#> AFR "AFR" "y2015" "B1"    
#> CPA "CPA" "y2015" "B1"    
#> EUR "EUR" "y2015" "B1"    
#> LAM "LAM" "y2015" "B1"    
#> MEA "MEA" "y2015" "B1"    
#> PAS "PAS" "y2015" "B1"    
#> SAS "SAS" "y2015" "B1"    
#> AFR "AFR" "y2025" "B1"    
#> CPA "CPA" "y2025" "B1"    
#> EUR "EUR" "y2025" "B1"    
#> LAM "LAM" "y2025" "B1"    
#> MEA "MEA" "y2025" "B1"    
#> PAS "PAS" "y2025" "B1"    
#> SAS "SAS" "y2025" "B1"    
#> AFR "AFR" "y2035" "B1"    
#> CPA "CPA" "y2035" "B1"    
#> EUR "EUR" "y2035" "B1"    
#> LAM "LAM" "y2035" "B1"    
#> MEA "MEA" "y2035" "B1"    
#> PAS "PAS" "y2035" "B1"    
#> SAS "SAS" "y2035" "B1"    
#> AFR "AFR" "y2045" "B1"    
#> CPA "CPA" "y2045" "B1"    
#> EUR "EUR" "y2045" "B1"    
#> LAM "LAM" "y2045" "B1"    
#> MEA "MEA" "y2045" "B1"    
#> PAS "PAS" "y2045" "B1"    
#> SAS "SAS" "y2045" "B1"    
#> AFR "AFR" "y2055" "B1"    
#> CPA "CPA" "y2055" "B1"    
#> EUR "EUR" "y2055" "B1"    
#> LAM "LAM" "y2055" "B1"    
#> MEA "MEA" "y2055" "B1"    
#> PAS "PAS" "y2055" "B1"    
#> SAS "SAS" "y2055" "B1"    
#> AFR "AFR" "y2065" "B1"    
#> CPA "CPA" "y2065" "B1"    
#> EUR "EUR" "y2065" "B1"    
#> LAM "LAM" "y2065" "B1"    
#> MEA "MEA" "y2065" "B1"    
#> PAS "PAS" "y2065" "B1"    
#> SAS "SAS" "y2065" "B1"    
#> AFR "AFR" "y2075" "B1"    
#> CPA "CPA" "y2075" "B1"    
#> EUR "EUR" "y2075" "B1"    
#> LAM "LAM" "y2075" "B1"    
#> MEA "MEA" "y2075" "B1"    
#> PAS "PAS" "y2075" "B1"    
#> SAS "SAS" "y2075" "B1"    
#> AFR "AFR" "y2085" "B1"    
#> CPA "CPA" "y2085" "B1"    
#> EUR "EUR" "y2085" "B1"    
#> LAM "LAM" "y2085" "B1"    
#> MEA "MEA" "y2085" "B1"    
#> PAS "PAS" "y2085" "B1"    
#> SAS "SAS" "y2085" "B1"    
#> AFR "AFR" "y2095" "B1"    
#> CPA "CPA" "y2095" "B1"    
#> EUR "EUR" "y2095" "B1"    
#> LAM "LAM" "y2095" "B1"    
#> MEA "MEA" "y2095" "B1"    
#> NAM "NAM" "y2095" "B1"    
#> PAS "PAS" "y2095" "B1"    
#> SAS "SAS" "y2095" "B1"    
#> AFR "AFR" "y2105" "B1"    
#> CPA "CPA" "y2105" "B1"    
#> EUR "EUR" "y2105" "B1"    
#> LAM "LAM" "y2105" "B1"    
#> MEA "MEA" "y2105" "B1"    
#> NAM "NAM" "y2105" "B1"    
#> PAS "PAS" "y2105" "B1"    
#> SAS "SAS" "y2105" "B1"    
#> AFR "AFR" "y2115" "B1"    
#> CPA "CPA" "y2115" "B1"    
#> EUR "EUR" "y2115" "B1"    
#> LAM "LAM" "y2115" "B1"    
#> MEA "MEA" "y2115" "B1"    
#> NAM "NAM" "y2115" "B1"    
#> PAS "PAS" "y2115" "B1"    
#> SAS "SAS" "y2115" "B1"    
#> AFR "AFR" "y2125" "B1"    
#> CPA "CPA" "y2125" "B1"    
#> EUR "EUR" "y2125" "B1"    
#> LAM "LAM" "y2125" "B1"    
#> MEA "MEA" "y2125" "B1"    
#> NAM "NAM" "y2125" "B1"    
#> PAS "PAS" "y2125" "B1"    
#> SAS "SAS" "y2125" "B1"    
#> AFR "AFR" "y2135" "B1"    
#> CPA "CPA" "y2135" "B1"    
#> EUR "EUR" "y2135" "B1"    
#> LAM "LAM" "y2135" "B1"    
#> MEA "MEA" "y2135" "B1"    
#> NAM "NAM" "y2135" "B1"    
#> PAS "PAS" "y2135" "B1"    
#> SAS "SAS" "y2135" "B1"    
#> AFR "AFR" "y2145" "B1"    
#> CPA "CPA" "y2145" "B1"    
#> EUR "EUR" "y2145" "B1"    
#> LAM "LAM" "y2145" "B1"    
#> MEA "MEA" "y2145" "B1"    
#> NAM "NAM" "y2145" "B1"    
#> PAS "PAS" "y2145" "B1"    
#> SAS "SAS" "y2145" "B1"    
#> 
#> $true$regions
#> [1] "AFR" "CPA" "EUR" "SAS" "LAM" "PAS" "MEA" "NAM"
#> 
#> $true$years
#>  [1] "y1995" "y2005" "y2015" "y2025" "y2035" "y2045" "y2055" "y2065"
#>  [9] "y2075" "y2085" "y2095" "y2105" "y2115" "y2125" "y2135" "y2145"
#> 
#> $true$data
#> [1] "A2" "B1"
#> 
#> 
#> $false
#> $false$individual
#>     i     t       scenario
#> FSU "FSU" "y1995" "A2"    
#> LAM "LAM" "y1995" "A2"    
#> MEA "MEA" "y1995" "A2"    
#> NAM "NAM" "y1995" "A2"    
#> PAO "PAO" "y1995" "A2"    
#> PAS "PAS" "y1995" "A2"    
#> FSU "FSU" "y2005" "A2"    
#> MEA "MEA" "y2005" "A2"    
#> NAM "NAM" "y2005" "A2"    
#> PAO "PAO" "y2005" "A2"    
#> FSU "FSU" "y2015" "A2"    
#> MEA "MEA" "y2015" "A2"    
#> NAM "NAM" "y2015" "A2"    
#> PAO "PAO" "y2015" "A2"    
#> FSU "FSU" "y2025" "A2"    
#> NAM "NAM" "y2025" "A2"    
#> PAO "PAO" "y2025" "A2"    
#> FSU "FSU" "y2035" "A2"    
#> NAM "NAM" "y2035" "A2"    
#> PAO "PAO" "y2035" "A2"    
#> FSU "FSU" "y2045" "A2"    
#> NAM "NAM" "y2045" "A2"    
#> PAO "PAO" "y2045" "A2"    
#> FSU "FSU" "y2055" "A2"    
#> NAM "NAM" "y2055" "A2"    
#> PAO "PAO" "y2055" "A2"    
#> FSU "FSU" "y2065" "A2"    
#> NAM "NAM" "y2065" "A2"    
#> PAO "PAO" "y2065" "A2"    
#> FSU "FSU" "y2075" "A2"    
#> NAM "NAM" "y2075" "A2"    
#> PAO "PAO" "y2075" "A2"    
#> FSU "FSU" "y2085" "A2"    
#> NAM "NAM" "y2085" "A2"    
#> PAO "PAO" "y2085" "A2"    
#> FSU "FSU" "y2095" "A2"    
#> PAO "PAO" "y2095" "A2"    
#> FSU "FSU" "y2105" "A2"    
#> PAO "PAO" "y2105" "A2"    
#> FSU "FSU" "y2115" "A2"    
#> PAO "PAO" "y2115" "A2"    
#> FSU "FSU" "y2125" "A2"    
#> PAO "PAO" "y2125" "A2"    
#> FSU "FSU" "y2135" "A2"    
#> PAO "PAO" "y2135" "A2"    
#> FSU "FSU" "y2145" "A2"    
#> PAO "PAO" "y2145" "A2"    
#> FSU "FSU" "y1995" "B1"    
#> LAM "LAM" "y1995" "B1"    
#> MEA "MEA" "y1995" "B1"    
#> NAM "NAM" "y1995" "B1"    
#> PAO "PAO" "y1995" "B1"    
#> PAS "PAS" "y1995" "B1"    
#> FSU "FSU" "y2005" "B1"    
#> MEA "MEA" "y2005" "B1"    
#> NAM "NAM" "y2005" "B1"    
#> PAO "PAO" "y2005" "B1"    
#> FSU "FSU" "y2015" "B1"    
#> NAM "NAM" "y2015" "B1"    
#> PAO "PAO" "y2015" "B1"    
#> FSU "FSU" "y2025" "B1"    
#> NAM "NAM" "y2025" "B1"    
#> PAO "PAO" "y2025" "B1"    
#> FSU "FSU" "y2035" "B1"    
#> NAM "NAM" "y2035" "B1"    
#> PAO "PAO" "y2035" "B1"    
#> FSU "FSU" "y2045" "B1"    
#> NAM "NAM" "y2045" "B1"    
#> PAO "PAO" "y2045" "B1"    
#> FSU "FSU" "y2055" "B1"    
#> NAM "NAM" "y2055" "B1"    
#> PAO "PAO" "y2055" "B1"    
#> FSU "FSU" "y2065" "B1"    
#> NAM "NAM" "y2065" "B1"    
#> PAO "PAO" "y2065" "B1"    
#> FSU "FSU" "y2075" "B1"    
#> NAM "NAM" "y2075" "B1"    
#> PAO "PAO" "y2075" "B1"    
#> FSU "FSU" "y2085" "B1"    
#> NAM "NAM" "y2085" "B1"    
#> PAO "PAO" "y2085" "B1"    
#> FSU "FSU" "y2095" "B1"    
#> PAO "PAO" "y2095" "B1"    
#> FSU "FSU" "y2105" "B1"    
#> PAO "PAO" "y2105" "B1"    
#> FSU "FSU" "y2115" "B1"    
#> PAO "PAO" "y2115" "B1"    
#> FSU "FSU" "y2125" "B1"    
#> PAO "PAO" "y2125" "B1"    
#> FSU "FSU" "y2135" "B1"    
#> PAO "PAO" "y2135" "B1"    
#> FSU "FSU" "y2145" "B1"    
#> PAO "PAO" "y2145" "B1"    
#> 
#> $false$regions
#> [1] "FSU" "LAM" "MEA" "NAM" "PAO" "PAS"
#> 
#> $false$years
#>  [1] "y1995" "y2005" "y2015" "y2025" "y2035" "y2045" "y2055" "y2065"
#>  [9] "y2075" "y2085" "y2095" "y2105" "y2115" "y2125" "y2135" "y2145"
#> 
#> $false$data
#> [1] "A2" "B1"
#> 
#> 
#> $na
#> $na$individual
#>      i t scenario
#> 
#> $na$regions
#> character(0)
#> 
#> $na$years
#> character(0)
#> 
#> $na$data
#> character(0)
#> 
#> 
#> $summary
#>  TRUE FALSE    NA other 
#>   227    93     0     0 
#> 
```
