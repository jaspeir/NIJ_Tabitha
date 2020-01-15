#' Function to determine whether x dominates y on criterion set P.
#' We assume q is of gain-type criterion.
#' @param x the left operand
#' @param y the right operand
#' @param P the set of criterions to test
#' @return whether x dominates y on criterion set P
#' @export
dominates = function(x, y, P) {
  stopifnot(nrow(x) == nrow(y), nrow(x) > 0, P %in% names(x), P %in% names(y))

  map_dfc(P, ~ outranks(x, y, .)) %>% apply(FUN = all, MARGIN = 1)
}
