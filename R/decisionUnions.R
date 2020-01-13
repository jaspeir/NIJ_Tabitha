#' Function for calculating the downward class union
#' @param examples the examples in the decision table
#' @param t the decision class to compare to
#' @return the set of objects in the downward class union
#' @export
downwardClassUnion = function(examples, t) {

  objectCol = names(examples)[1]
  decisionCol = names(examples)[ncol(examples)]

  examples %>%
    filter(get(decisionCol) <= t) %>%
    pull(get(objectCol))
}

#' Function for calculating the upward class union
#' @param examples the examples in the decision table
#' @param t the decision class to compare to
#' @return the set of objects in the upward class union
#' @export
upwardClassUnion = function(examples, t) {

  objectCol = names(examples)[1]
  decisionCol = names(examples)[ncol(examples)]

  examples %>%
    filter(get(decisionCol) >= t) %>%
    pull(get(objectCol))
}
