#' R6 class representing a complex condition.
#'
#' @description
#' A complex condition is made up of multiple elementary conditions by forming their conjunction.
#'
#' @details
#' A complex condition is the conjunction of elementary conditions.
#'
#' @export
ComplexCondition <- R6::R6Class(
  classname = "ComplexCondition",

  public = list(

    #' @field conditions the set elementary conditions
    conditions = NA,

    #' @description
    #' Create a new ComplexCondition object.
    #' @param conditions the set of elementary conditions
    initialize = function(conditions) {

      # ERROR-CHECKS:
      stopifnot('ElementaryCondition' %in% class(conditions))
      stopifnot('InformationTable' %in% class(it))
      self$conditions = conditions
    },

    #' @description
    #' Method to calculate the set of objects matching this complex condition.
    #' @param it the information table to use
    #' @return the set of matching objects - logical vector
    complexCover = function(it) {

      coveredEach = map_dfr(conditions, function(elem) {elem$elementCover(it) })
      covered = apply(coveredEach, MARGIN = 2, FUN = all)

      return(covered)
    },

    #' @description
    #' Method for calculating the first comparison metric used in the DOMLEM paper.
    #' @param G the parameter of the metric - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a numeric metric between 0 and 1. Higher is better.
    firstMetric = function(G, it) {
      covered = self$complexCover(it)

      # ERROR-CHECKS:
      stopifnot(length(G) == length(covered))

      return(sum(covered & G) / sum(covered))
    },

    #' @description
    #' Method for calculating the second comparison metric used in the DOMLEM paper.
    #' @param G the parameter of the metric - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a non-negative integer metric. Higher is better.
    secondMetric = function(G, it) {
      covered = self$complexCover(it)

      # ERROR-CHECKS:
      stopifnot(length(G) == length(covered))

      return(sum(covered & G))
    },

    #' @description
    #' Print method.
    print = function() {
      cat(paste(conditions, sep = " AND "))

      invisible(self)
    }
  )
)
