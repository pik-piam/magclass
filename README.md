# Data Class and Tools for Handling Spatial-Temporal Data

R package **magclass**, version **5.9.1**

[![Travis build status](https://travis-ci.com/pik-piam/magclass.svg?branch=master)](https://travis-ci.com/pik-piam/magclass) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158580.svg)](https://doi.org/10.5281/zenodo.1158580) [![codecov](https://codecov.io/gh/pik-piam/magclass/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/magclass)

## Purpose and Functionality

Data class for increased interoperability working with spatial-
    temporal data together with corresponding functions and methods (conversions,
    basic calculations and basic data manipulation). The class distinguishes
    between spatial, temporal and other dimensions to facilitate the development
    and interoperability of tools build for it. Additional features are name-based
    addressing of data and internal consistency checks (e.g. checking for the right
    data order in calculations).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magclass")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **magclass** in publications use:

Dietrich J, Bodirsky B, Bonsch M, Humpenoeder F, Bi S, Karstens K
(2020). _magclass: Data Class and Tools for Handling Spatial-Temporal
Data_. doi: 10.5281/zenodo.1158580 (URL:
https://doi.org/10.5281/zenodo.1158580), R package version 5.9.1, <URL:
https://github.com/pik-piam/magclass>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {magclass: Data Class and Tools for Handling Spatial-Temporal Data},
  author = {Jan Philipp Dietrich and Benjamin Leon Bodirsky and Markus Bonsch and Florian Humpenoeder and Stephen Bi and Kristine Karstens},
  year = {2020},
  note = {R package version 5.9.1},
  doi = {10.5281/zenodo.1158580},
  url = {https://github.com/pik-piam/magclass},
}
```

