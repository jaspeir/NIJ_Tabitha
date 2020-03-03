#' Function to determine whether x is similar to y on attribute q.
#' The alpha- and beta-parameters of the similarity metric should be provided as attributes of column q.
#' @param x the left operand
#' @param y the right operand
#' @param q the attribute to test
#' @return whether x is similar to y on attribute q
#' @export
similar = function(x, y, q) {
  stopifnot(nrow(x) == nrow(y), nrow(x) > 0, q %in% names(x), q %in% names(y), !is.null(attributes(pull(y, !!q))$alpha), !is.null(attributes(pull(y, !!q))$beta))

  exampleX = pull(x, !!q)
  exampleY = pull(y, !!q)
  alpha = attributes(exampleY)$alpha
  beta = attributes(exampleY)$beta

  abs(exampleX - exampleY) <= alpha * exampleY + beta
}
