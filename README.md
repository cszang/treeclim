[![Build Status](https://travis-ci.org/cszang/treeclim.svg?branch=master)](https://travis-ci.org/cszang/treeclim)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/treeclim)](http://cran.r-project.org/package=treeclim)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/treeclim)](http://www.r-pkg.org/pkg/treeclim)
[![Coverage Status](https://img.shields.io/codecov/c/github/cszang/treeclim/master.svg)](https://codecov.io/github/cszang/treeclim?branch=master)
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

See the [docs](http://cszang.github.com/treeclim) and the
[wiki](https://github.com/cszang/treeclim/wiki) for details.

## Citation

Zang, C., and F. Biondi. 2015. treeclim: an R package for the
numerical calibration of proxy-climate relationships. Ecography
38:431–436.
