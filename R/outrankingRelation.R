#' Function to determine whether x outranks y on criterion q.
#' We assume q is of gain-type criterion.
#' @param x the left operand
#' @param y the right operand
#' @param q the criterion to test
#' @return whether x outranks y on criterion q
#' @export
outranks = function(x, y, q) {
  stopifnot(nrow(x) == nrow(y), nrow(x) > 0, q %in% names(x), q %in% names(y))

  pull(x, !!q) >= pull(y, !!q)
}
