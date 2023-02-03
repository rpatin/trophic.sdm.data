trophic_sdm_data: preparing data for European Vertebrates trophic SDM
================

<!-- [![](https://www.r-pkg.org/badges/version/segclust2d?color=orange)](https://cran.r-project.org/package=segclust2d) -->
<!-- [![](http://cranlogs.r-pkg.org/badges/grand-total/segclust2d?color=yellow)](https://cran.r-project.org/package=segclust2d) -->

[![](https://img.shields.io/badge/devel%20version-0.0.1-blue.svg)](https://github.com/rpatin/trophic_sdm_data)
[![](https://img.shields.io/github/last-commit/rpatin/trophic_sdm_data.svg)](https://github.com/rpatin/trophic_sdm_data/commits/main)

# Introduction

# News

Pre-Release Features (v. 0.0.1) - detection of taxonomic conflicts
within trophic groups with `detect_taxonomic_conflict()`. i.e. predator
with some prey within the same trophic groups but with very different
taxonomy (that would be problematic to mix in a trophic sdm) - detection
of gbif observation outside of IUCN ranges and diagnostic of the
distance to IUCN distribution. - calculation of gbif sampling effort -
rasterization and buffer calculation for IUCN polygon ranges

# Installation

If you want the newest
[![](https://img.shields.io/badge/devel%20version-0.0.1-blue.svg)](https://github.com/rpatin/trophic_sdm_data),
you can install `trophic_sdm_data` from github with:

``` r
devtools::install_github("rpatin/trophic_sdm_data")
```
