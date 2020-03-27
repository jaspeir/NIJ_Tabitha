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

      # Handle empty complex rule:
      if (self$length() == 0) {
        return(rep(FALSE, length(it$objects)))
      }

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
    #' Method for calculating all comparison metrics used in the DOMLEM paper.
    #' @param G the parameter of the metrics - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a list of metrics. Higher is better.
    allMetrics = function(G, it) {
      return(list(first = self$firstMetric(G, it), second = self$secondMetric(G, it)))
    },

    #' @description
    #' Method to evaluate if an elementary condition is better than the current best.
    #' @param G the parameter of the metric - a vector describing a set of objects
    #' @param it the information table to use
    #' @param check the elementary condition to evaluate
    #' @param best the current best elementary
    #' @return the better of the two elementary conditions
    findBestElementary = function(G, it, check, best) {

      if (is.na(best) || is.null(best)) {
        return(check)
      }

      tempCheck = ComplexCondition$new(c(self$conditions, check))
      tempBest = ComplexCondition$new(c(self$conditions, best))

      checkMetric = tempCheck$allMetrics(G, it)
      bestMetric = tempBest$allMetrics(G, it)

      if (checkMetric$first > bestMetric$first ||
          (checkMetric$first > bestMetric$first && checkMetric$second >= bestMetric$second)) {
        return(check)
      } else {
        return(best)
      }
    },

    #' @description
    #' Method that tries to make a complex condition shorter.
    #' For each elementary condition e in E, check if [E - {e}] subset or equal B then E := E - {e}.
    #' @param B the objects to cover - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a complex condition
    reduceConditions = function(B, it) {

      removedConditions = rep(FALSE, length(self$conditions))  # which elementary condition has been removed already

      for (i in seq_along(self$conditions)) {
        temp = ComplexCondition$new(self$conditions[!removedConditions])
        tempCover = temp$complexCover(it)
        if (isSubset(tempCover, B)) {
          removedConditions[i] = TRUE
        }
      }

      return(ComplexCondition$new(self$conditions[!removedConditions]))
    },

    #' @description
    #' Method for appending an additional elementary condition to the current conditions.
    #' @param elem the elementary condition to be added
    append = function(elem) {
      self$conditions = c(self$conditions, elem)
    },

    #' @description
    #' Method that returns the number of elementary conditions in this complex condition.
    length = function() {
      return(length(self$conditions))
    },

    #' @description
    #' Print method.
    print = function() {
      cat(paste(conditions, sep = " AND "))

      invisible(self)
    }
  )
)