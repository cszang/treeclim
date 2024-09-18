[![R-CMD-check](https://github.com/cszang/treeclim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cszang/treeclim/actions/workflows/R-CMD-check.yaml)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/treeclim)](https://cran.r-project.org/package=treeclim)
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/grand-total/treeclim)](https://www.r-pkg.org/pkg/treeclim)
[![Codecov test coverage](https://codecov.io/gh/cszang/treeclim/graph/badge.svg)](https://app.codecov.io/gh/cszang/treeclim)
[![MIT licensed](https://img.shields.io/badge/license-GPL%20%3E%3D%203-yellowgreen.svg)](https://github.com/cszang/treeclim/blob/master/DESCRIPTION)

# treeclim

An R package for modelling tree/climate relationships. The package
features:

- static, moving, and evolving response and correlation functions
- seasonal correlations
- a "dendro-flavoured" linear model
- evaluation of reconstruction skills
- a test for spurious moving correlations
- nice default plots

## Usage

```R
library(treeclim)
munich_spruce_calib <- dcc(muc_spruce, muc_clim)
plot(munich_spruce_calib)
skills(munich_spruce_calib)
```

See the [docs](https://www.rdocumentation.org/packages/treeclim/) and the
[wiki](https://github.com/cszang/treeclim/wiki) for details.

## Citation

Zang, C., and F. Biondi. 2015. treeclim: an R package for the
numerical calibration of proxy-climate relationships. Ecography
38:431–436.
