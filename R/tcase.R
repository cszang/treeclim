tcase <- function(x) {
  sapply(x, function(y) {
    ly <- nchar(y)
    paste0(toupper(substr(y, 1, 1)), substr(y, 2, ly))
  }, USE.NAMES = FALSE)
}
