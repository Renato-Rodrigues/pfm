# Political Feasibility Module (PFM) for IAMs

R package **pfm**, version **0.2.0**

   [![R build status](https://github.com/pik-piam/pfm/workflows/check/badge.svg)](https://github.com/pik-piam/pfm/actions) [![codecov](https://codecov.io/gh/pik-piam/pfm/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/pfm) 

## Purpose and Functionality

Econometric model for the political feasibility of carbon pricing in integrated
    assessment models. Implements the two-stage hurdle model (adoption + stringency) for
    Bulk and Diffuse sectors. Consumes magpie objects from mrpfm.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("pfm")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **pfm** in publications use:

Rodrigues R, Kriegler E (2026). "pfm: Political Feasibility Module (PFM) for IAMs." Version: 0.2.0, <https://github.com/pik-piam/pfm>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {pfm: Political Feasibility Module (PFM) for IAMs},
  author = {Renato Rodrigues and Elmar Kriegler},
  date = {2026-05-18},
  year = {2026},
  url = {https://github.com/pik-piam/pfm},
  note = {Version: 0.2.0},
}
```
