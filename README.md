# Data Class and Tools for Handling Spatial-Temporal Data

R package **magclass**, version **6.11.2**

[![CRAN status](https://www.r-pkg.org/badges/version/magclass)](https://cran.r-project.org/package=magclass) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158580.svg)](https://doi.org/10.5281/zenodo.1158580) [![R build status](https://github.com/pik-piam/magclass/workflows/check/badge.svg)](https://github.com/pik-piam/magclass/actions) [![codecov](https://codecov.io/gh/pik-piam/magclass/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/magclass) [![r-universe](https://pik-piam.r-universe.dev/badges/magclass)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Data class for increased interoperability working with
    spatial-temporal data together with corresponding functions and
    methods (conversions, basic calculations and basic data manipulation).
    The class distinguishes between spatial, temporal and other dimensions
    to facilitate the development and interoperability of tools build for
    it. Additional features are name-based addressing of data and internal
    consistency checks (e.g. checking for the right data order in
    calculations).


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

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("magclass-concept")   # Concept behind magclass
vignette("magclass-expansion") # MAGPIE Class Object Expansion
vignette("magclass")           # MAGPIE Class Tutorial
vignette("magclass6")          # magclass 6
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **magclass** in publications use:

Dietrich J, Bodirsky B, Bonsch M, Humpenoeder F, Bi S, Karstens K, Leip D (2023). _magclass: Data Class and Tools for Handling Spatial-Temporal Data_. doi:10.5281/zenodo.1158580 <https://doi.org/10.5281/zenodo.1158580>, R package version 6.11.2, <https://github.com/pik-piam/magclass>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {magclass: Data Class and Tools for Handling Spatial-Temporal Data},
  author = {Jan Philipp Dietrich and Benjamin Leon Bodirsky and Markus Bonsch and Florian Humpenoeder and Stephen Bi and Kristine Karstens and Debbora Leip},
  year = {2023},
  note = {R package version 6.11.2},
  doi = {10.5281/zenodo.1158580},
  url = {https://github.com/pik-piam/magclass},
}
```
