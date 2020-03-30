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
      if (missing(conditions)) {
        self$conditions = c()
      } else {
        if ('ElementaryCondition' %in% class(conditions)) {
          self$conditions = c(conditions)
        } else {
          walk(conditions, ~ stopifnot('ElementaryCondition' %in% class(.)))
          self$conditions = conditions
        }
      }
    },

    #' @description
    #' Method to calculate the set of objects matching this complex condition.
    #' @param it the information table to use
    #' @return the set of matching objects - set of object names
    complexCover = function(it) {

      # Handle empty complex rule:
      if (self$length() == 0) {
        return(it$objects)
      }

      covered = it$objects
      walk(self$conditions, function(elem) {
        covered <<- intersect(covered, elem$elementCover(it))
      })

      return(covered)
    },

    #' @description
    #' Method for calculating the first comparison metric used in the DOMLEM paper.
    #' @param G the parameter of the metric - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a numeric metric between 0 and 1. Higher is better.
    firstMetric = function(G, it) {
      covered = self$complexCover(it)

      if (length(covered) == 0) {
        return(0)
      }

      return(length(intersect(covered, G)) / length(covered))
    },

    #' @description
    #' Method for calculating the second comparison metric used in the DOMLEM paper.
    #' @param G the parameter of the metric - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a non-negative integer metric. Higher is better.
    secondMetric = function(G, it) {
      covered = self$complexCover(it)

      if (length(covered) == 0) {
        return(0)
      }

      return(length(intersect(covered,G)))
    },

    #' @description
    #' Method for calculating all comparison metrics used in the DOMLEM paper.
    #' @param G the parameter of the metrics - a vector describing a set of objects
    #' @param it the information table to use
    #' @return a list of metrics. Higher is better.
    allMetrics = function(G, it) {
      # ERROR-CHECKS:
      stopifnot("character" %in% class(G))
      stopifnot("InformationTable" %in% class(it))

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

      stopifnot("ElementaryCondition" %in% class(check))
      if (!"ElementaryCondition" %in% class(best)) {
        return(check)
      }

      tempCheck = ComplexCondition$new(c(self$conditions, check))
      tempBest = ComplexCondition$new(c(self$conditions, best))

      checkMetric = tempCheck$allMetrics(G, it)
      bestMetric = tempBest$allMetrics(G, it)

      if (checkMetric$first > bestMetric$first ||
          (checkMetric$first == bestMetric$first && checkMetric$second >= bestMetric$second)) {
        return(check)
      } else {
        return(best)
      }
    },

    #' @description
    #' Method that tries to make a complex condition shorter.
    #' For each elementary condition e in E, check if [E - {e}] subset or equal B then E := E - {e}.
    #' @param B the objects to cover - set of object names
    #' @param it the information table to use
    #' @return a complex condition
    reduceConditions = function(B, it) {

      removedConditions = rep(FALSE, length(self$conditions))  # which elementary condition has been removed already

      for (i in seq_along(self$conditions)) {
        tempRemoved = removedConditions
        tempRemoved[i] = TRUE

        if (sum(!tempRemoved) == 0) {
          break()
        }

        temp = ComplexCondition$new(self$conditions[!tempRemoved])
        tempCover = temp$complexCover(it)
        if (isSubsetArbitrary(tempCover, B)) {
          removedConditions[i] = TRUE
        }
      }

      return(ComplexCondition$new(self$conditions[!removedConditions]))
    },

    #' @description
    #' Method for creating an efficient representation of the constants used in the filter conditions.
    #' @param it the information table to use
    #' @return a vector of filter values. Not filtered attributes have an NA value.
    getConstants = function(it) {

      constants = rep(NA, nrow(it$metaData))
      if (self$length() == 0) {
        return(constants)
      }

      # NOTE: we do not handle the case when there are multiple elementary conditions on the same attribute
      activeConstants = map_dfr(self$conditions, function(cond) { list(name = cond$attribute, value = as.character(cond$value))})
      attributeIndexes = map_int(activeConstants$name, ~ which(. == it$metaData$name, arr.ind = TRUE))
      constants[attributeIndexes] = activeConstants$value

      return(constants)
    },

    #' @description
    #' Method for creating an efficient representation of the constants used in the filter conditions, grouped by the type of variable and condition.
    #' @param it the information table to use
    #' @return a list of vector of filter values, one list for the following operator groups: (= or ~). (>=), and (<=). Not filtered attributes have an NA value.
    getConstantsGrouped = function(it) {

      constantsPerGroup = rep(NA, nrow(it$metaData))
      constants = list(
        lowerBounds = constantsPerGroup,
        upperBounds = constantsPerGroup,
        others = constantsPerGroup
      )

      if (self$length() == 0) {
        return(constants)
      }

      # NOTE: we do not handle the case when there are multiple elementary conditions on the same attribute
      activeConstants = map_dfr(self$conditions, function(cond) {
        list(name = cond$attribute, isLowerBound = cond$isLowerBound, value = as.character(cond$value))
      })

      getAttributeIndexes = function(isLowerBound) {
        activeConstantsFiltered = if (is.na(isLowerBound)) {
          activeConstants %>% filter(is.na(isLowerBound))
        } else if (isLowerBound) {
          activeConstants %>% filter(isLowerBound)
        } else {
          activeConstants %>% filter(!isLowerBound)
        }
        map_int(activeConstantsFiltered$name, ~ which(. == it$metaData$name, arr.ind = TRUE))
      }

      constants$others[getAttributeIndexes(NA)] = activeConstants %>% filter(is.na(isLowerBound)) %>% pull(value)
      constants$lowerBounds[getAttributeIndexes(TRUE)] = activeConstants %>% filter(isLowerBound) %>% pull(value)
      constants$upperBounds[getAttributeIndexes(FALSE)] = activeConstants %>% filter(!isLowerBound) %>% pull(value)

      return(constants)
    },

    #' @description
    #' Method for deciding whether a provided elementary condition is part of this complex condition.
    #' @param elem the elementary condition to check
    #' @return a boolean value
    contains = function(elem) {
      results = map_lgl(self$conditions, function(e) e$equals(elem) )

      return(any(results))
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
    #' print method.
    print = function() {
      cat(self$toString())
      invisible(self)
    },

    #' @description
    #' toString method.
    toString = function() {
      paste(map_chr(self$conditions, ~ .$toString()), collapse = " AND ")
    },

    #' @description
    #' Method to compare two ComplexConditions.
    #' @param other the other complex condition to compare to
    #' @return a single boolean value
    equals = function(other) {

      if (any(is.null(other))) {
        return(FALSE)
      }
      stopifnot("ComplexCondition" %in% class(other))

      if (self$length() != other$length()) {
        return(FALSE)
      }

      equal = map2_lgl(self$conditions, other$conditions, function(a, b) {a$equals(b)})

      return(all(equal))
    }
  )
)
