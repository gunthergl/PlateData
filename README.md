# PlateData
![development status](https://img.shields.io/badge/status-under_development-orange)
![GitHub Downloads (all assets, all releases)](https://img.shields.io/github/downloads/OliverDietrich/microtiter-plate-analysis-toolkit/total)

R package for the analysis of microtiter plate-based data. Data must be attributable to a specific well (e.g. A1) in a plate (e.g. P1). The PlateData object stores the layout and data as data.frames connected by an index column. The plate type (e.g. 6-well, 24-well, 96-well is determined automatically and stored as 'type').

<img src="img/overview.png"/>

## Installation
There is no official release version yet, since the package is under development.

You can install the development version from GitHub using the `remotes` package:
```
remotes::install_github("OliverDietrich/microtiter-plate-analysis-toolkit@main")
```
or the even less stable but more up-to-date
```
remotes::install_github("OliverDietrich/microtiter-plate-analysis-toolkit@develop")
```

> [!WARNING]
> Early development, no stable features.
> 
## ToDo
- [ ] Get example data for import methods
  - [ ] Layout
  - [ ] Tekan Spark
- [ ] Write tutorial for simple timecourse experiment (design + analysis)
- [ ] Create development branch to stabilize main


## Working on it, GG
```r
gitcreds::gitcreds_set() # Then enter the freshly generated token
dir.create("/home/gugl/clonedgit/others/PlateData")
usethis::create_from_github(
    repo="OliverDietrich/PlateData", 
    fork = TRUE, 
    destdir = "/home/gugl/clonedgit/others/PlateData"
)

usethis::use_tidy_github()
usethis::use_tidy_github_actions()
# Which repo should we target? 
# 1: gunthergl/PlateData = 'origin'
# 2: OliverDietrich/PlateData = 'upstream'
# Selection: 1
usethis::use_github_action("check-standard")
usethis::use_tidy_github_labels()
usethis::use_pkgdown_github_pages()

```