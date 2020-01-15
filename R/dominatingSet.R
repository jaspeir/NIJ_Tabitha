#' This function calculates the dominating set of an object with respect to a criterion set.
#' @param x the object
#' @param P the criterion set
#' @param examples the examples in the decision table
#' @return the set of objects that dominate object x
#' @export
dominatingSet = function(x, P, examples) {

  objectCol = 1
  object = examples[examples[, objectCol] == x, ]
  stopifnot(nrow(object) == 1)

  d = map_lgl(seq(nrow(examples)), ~ dominates(examples[., ], object, P))

  pull(examples[d, objectCol])
}

#' This function calculates the dominated set of an object with respect to a criterion set.
#' @param x the object
#' @param P the criterion set
#' @param examples the examples in the decision table
#' @return the set of objects that are dominated by object x
#' @export
dominatedSet = function(x, P, examples) {

  objectCol = 1
  object = examples[examples[, objectCol] == x, ]
  stopifnot(nrow(object) == 1)

  d = map_lgl(seq(nrow(examples)), ~ dominates(object, examples[., ], P))

  pull(examples[d, objectCol])
}
