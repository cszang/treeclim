# treeclim 2.0.8.0

* Fix: avoid spurious -Warray-bounds warning under GCC 16 by using fill::none for fully-populated result matrices (pcor.cpp, corfun.cpp, ...)
* Modernise src/Makevars: drop deprecated LdFlags/RcppLdFlags calls (no-ops since Rcpp 0.11.0, 2013)
* Tidy DESCRIPTION: move Rcpp to Imports, drop pseudo-dependencies on base/stats/utils