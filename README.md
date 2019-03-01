# R magclass package

## Purpose and Functionality

The magclass package provides a data class for increased interoperability working with spatial-temporal data together with corresponding functions and methods (conversions, basic calculations and basic data manipulation). The class distinguishes    between spatial, temporal and other dimensions to facilitate the development and interoperability of tools build for it. Additional features are name-based addressing of data and internal consistency checks (e.g. checking for the right data order in calculations).


## Installation

The last official release of the package can be directly installed from official CRAN repository:

```r 
install.packages("magclass")
```

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magclass")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r 
vignette("magclass")
```


## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/magclass.svg?branch=master)](https://travis-ci.com/pik-piam/magclass)


## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.


## Citation

[![DOI](https://zenodo.org/badge/93050064.svg)](https://zenodo.org/badge/latestdoi/93050064)

