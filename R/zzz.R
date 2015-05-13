.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()
  
  tips <- c(
    "Need help? See the wiki at https://github.com/cszang/treeclim/wiki",
    "Coming from bootRes? See here for a list of changes and for how to adapt your code: https://github.com/cszang/treeclim/wiki/Changes-from-bootRes-1.X",
    "See citation('treeclim') for information how to cite this package in your work."
  )
  supp <- "(Use suppressPackageStartupMessages to eliminate package startup messages.)"
  tip <- sample(tips, 1)
  the_tip <- paste(tip, supp, sep = "\n")
  packageStartupMessage(the_tip)
}