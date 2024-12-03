# The liMES R package

R package **limes**, version **2.68.0**

[![CRAN status](https://www.r-pkg.org/badges/version/limes)](https://cran.r-project.org/package=limes)  [![R build status](https://github.com/pik-piam/limes/workflows/check/badge.svg)](https://github.com/pik-piam/limes/actions) [![codecov](https://codecov.io/gh/pik-piam/limes/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/limes) [![r-universe](https://pik-piam.r-universe.dev/badges/limes)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Contains the LIMES-specific routines for data and model
    output manipulation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("limes")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("compareScenariosLimes") # compareScenariosLimes
```

## Questions / Problems

In case of questions / problems please contact Sebastian Osorio <sebastian.osorio@pik-potsdam.de>.

## Citation

To cite package **limes** in publications use:

Osorio S (2024). _limes: The liMES R package_. R package version 2.68.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {limes: The liMES R package},
  author = {Sebastian Osorio},
  year = {2024},
  note = {R package version 2.68.0},
}
```
