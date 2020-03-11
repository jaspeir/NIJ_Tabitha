#' Function to determine whether x dominates y on criterion set P.
#' We assume q is of gain-type criterion.
#' @param x the left operand
#' @param y the right operand
#' @param P the set of criterions to test
#' @return whether x dominates y on criterion set P
#' @export
dominatesOutrankOnly = function(x, y, P) {
  stopifnot(nrow(x) == nrow(y), nrow(x) > 0, P %in% names(x), P %in% names(y))

  map_dfc(P, ~ outranks(x, y, .)) %>% apply(FUN = all, MARGIN = 1)
}

#' Function to determine whether x dominates y on the mixed attribute set P.
#' @param x the left operand
#' @param y the right operand
#' @param P the set of attributes to test
#' @return whether x dominates y on attribute set P
#' @export
dominates = function(x, y, P) {
  stopifnot(nrow(x) == nrow(y), nrow(x) > 0, P %in% names(x), P %in% names(y))

  types = map_chr(P, function(q) {
    result = attributes(pull(y, !!q))$type
    ifelse(is.null(result), "", result)
  })
  P_ind = P[types == "indiscernibility" || types == ""]
  P_sim = P[types == "similarity"]
  P_dom = P[types == "dominance"]

  R_ind = map_dfc(P_ind, function(q) {pull(x, !!q) == pull(y, !!q)}) %>% apply(FUN = all, MARGIN = 1)
  R_sim = map_dfc(P_sim, ~ similar(x, y, .)) %>% apply(FUN = all, MARGIN = 1)
  R_dom = map_dfc(P_dom, ~ outranks(x, y, .)) %>% apply(FUN = all, MARGIN = 1)

  if (length(R_ind) == 0) {
    R_ind = rep(TRUE, nrow(x))
  }
  if (length(R_sim) == 0) {
    R_sim = rep(TRUE, nrow(x))
  }
  if (length(R_dom) == 0) {
    R_dom = rep(TRUE, nrow(x))
  }

  R_ind & R_sim & R_dom
}
