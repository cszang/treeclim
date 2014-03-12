.onAttach <- function(...) {
  library(help=bootres2)$info[[1]] -> version
  version <- version[pmatch("Version",version)]
  um <- strsplit(version," ")[[1]]
  version <- um[nchar(um) > 0][2]
  hello <- paste("This is bootres2 version ", version, ".", sep = "")
  afool <- substr(date(), 5, 10) == "Apr 01"
  afoolt <- ifelse(afool,
                   "You should not wear this shirt for work...\n", "")
  tips <- c(
    "Need help? See the wiki at https://github.com/znag/bootres2/wiki",
    paste("Coming from bootRes 1.X? See here for a list of changes and for how to adapt your code: https://github.com/znag/bootres2/wiki/Changes-from-bootRes-1.X"),
    "See citation('bootres2') for information how to cite this package in your work." 
  )
  supp <- "(Use suppressPackageStartupMessages to eliminate package startup messages.)"
  tip <- sample(tips, 1)
  the_tip <- paste(hello, afoolt, tip, supp, sep = "\n")
  packageStartupMessage(the_tip)
}
