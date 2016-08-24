#' unify selections to handle "classic" (bootRes-style) selections
#'
#' @param selection a treeclim selection
#'
#' @return unified (list-style) selection
#'
#' @keywords internal
unify_selection <- function(selection) {
  ## when numeric vector = "classic" (bR 1.X) parameter selection
  if (is.numeric(selection)) {
    selection <- eval(
      substitute(
        .range(.months = sel, .variables = NULL),
        list(sel = selection)
      )
    )
  }
  selection
}