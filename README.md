trophic.sdm.data: preparing data for European Vertebrates trophic SDM
================

<!-- [![](https://www.r-pkg.org/badges/version/segclust2d?color=orange)](https://cran.r-project.org/package=segclust2d) -->
<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/segclust2d?color=yellow)](https://cran.r-project.org/package=segclust2d) -->

[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/rpatin/trophic.sdm.data)
[![](https://img.shields.io/github/last-commit/rpatin/trophic.sdm.data.svg)](https://github.com/rpatin/trophic.sdm.data/commits/main)

# Introduction

# News

## v0.1.0 First Release (29/08/2023)

- Fixed and completed documentation
- Bugfix to summary function

## v0.0.5 (pre-release)

### Features

- calculation of aggregated prey metrics
- mixed workflow (backup IUCN) is possible to parameterize:
  - when not enough occurrences are present in gbif
  - when gbif extraction failed
  - when certainty threshold are too low
  - when there are too high proportion of uncertain cell within IUCN
    range
- improved and speed up workflow using raster to calculate aggregated
  prey metrics
- improved show method for trophic_dataset
- added overwrite options

## v0.0.4 (12/06/2023)

- support for multiple resolutions
- workflow for IUCN
- improved subsampling
- improved workflow

## v0.0.3

- subsampling of absences outside IUCN distribution
- support for filtering atlas data

## v0.0.2 - Bugfix Release (22/02/2023)

### bugfix

- threshold are now always discrete values
- buffer_iucn only create non-existing files

## v0.0.1 - First Release (21/02/2023)

### Release feature

- workflow for trophic dataset based on gbif data
- detection of taxonomic conflicts within trophic groups with
  `detect_taxonomic_conflict()`. i.e. predator with some prey within the
  same trophic groups but with very different taxonomy (that would be
  problematic to mix in a trophic sdm)
- detection of gbif observation outside of IUCN ranges and diagnostic of
  the distance to IUCN distribution.
- calculation of gbif sampling effort
- buffer calculation for IUCN polygon ranges

### Missing features

- no support for filtering atlas data & multiple resolutions
- no workflow for IUCN
- no evaluations
- no subsampling
- no calculation of aggregated prey metrics
- no splitting of trophic group in metaweb

# Installation

If you want the newest
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/rpatin/trophic.sdm.data),
you can install `trophic.sdm.data` from github with:

``` r
devtools::install_github("rpatin/trophic.sdm.data")
```
