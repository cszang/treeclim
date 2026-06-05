## Resubmission
   
This release addresses the GCC 16 -Warray-bounds warning flagged 
on r-devel-linux-x86_64-debian-gcc, plus deprecation warnings from 
Rcpp:::LdFlags() / RcppArmadillo:::RcppLdFlags() in src/Makevars.
   
## R CMD check results
   
0 errors | 0 warnings | 0 notes
   
## Test environments
   
* local: R 4.5.2 on macOS
* R-hub2: linux (debian-gcc-devel), macos, windows
* win-builder: R-devel