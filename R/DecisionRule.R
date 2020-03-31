#' R6 class representing a decision rule.
#'
#' @description
#' A decision rule is an implication extracted from an information table.
#'
#' @details
#' A decision rule is made up of a complex condition and class assignments.
#' For STAT1 type of rules the conditions are lower bounds, and assignment goes to an upward class union.
#' For STAT2 type of rules the conditions are upper bounds, and assignment goes to a downward class union.
#'
#' @export
DecisionRule <- R6::R6Class(
  classname = "DecisionRule",

  public = list(

    #' @field condition the left-hand side of the implication - a complex condition
    condition = NA,

    #' @field t the right-hand side of the implication - t-parameter of class union(s)
    t = NA,

    #' @field type the right-hand side of the implication - (class union or union of classes)
    type = NA,

    #' @description
    #' Create a new DecisionRule object.
    #' @param condition the complex condition
    #' @param t the t-parameter of the class union(s)
    #' @param type the type of the rule
    initialize = function(condition, t, type) {

      # ERROR-CHECKS:
      stopifnot('ComplexCondition' %in% class(condition))
      stopifnot(any(c('numeric', 'integer', 'factor') %in% class(t)))
      stopifnot(length(type) == 1 && type %in% c('STAT1', 'STAT2', 'STAT3'))
      stopifnot(length(t) == 1 && type %in% c('STAT1', 'STAT2') ||
                  length(t) > 1 && type == 'STAT3')

      self$condition = condition
      self$t = t
      self$type = type
    },

    #' @description
    #' Method to decide if this decision rule is minimal among the provided set of rules.
    #' @param it the information table to use
    #' @param rules the set of rules to check the minimality in
    #' @return a boolean value
    isMinimal = function(it, rules) {

      for (rule in rules) {
        if (self$isWeaker(it, rule)) {
          return(FALSE)
        }
      }

      return(TRUE)
    },

    #' @description
    #' Method to decide if this decision rule is a weaker implication compared to the provided rule.
    #' @param it the information table to use
    #' @param rule the rule to compare to
    #' @return a boolean value
    isWeaker = function(it, rule) {

      if (rule$type != self$type) {
        return(FALSE)
      }

      compareWithinGroup = function(other, this, group) {
        otherValues = other[[group]]
        otherAttributes = !is.na(otherValues)
        thisValues = this[[group]]
        thisAttributes = !is.na(thisValues)

        if (!isSubset(otherAttributes, thisAttributes)) {
          return(FALSE)
        }

        return(
        switch(group,
          'others' =      TRUE,
          'lowerBounds' = all(otherValues[otherAttributes] <= thisValues[otherAttributes]),
          'upperBounds' = all(otherValues[otherAttributes] >= thisValues[otherAttributes])
        ))
      }

      otherValuesGrouped = rule$condition$getConstantsGrouped(it)
      thisValuesGrouped = self$condition$getConstantsGrouped(it)

      # If only non-dominance attributes are present, then we cannot compare:
      if (all(is.na(c(thisValuesGrouped$lowerBounds, thisValuesGrouped$upperBounds)))) {
        return(FALSE)
      }

      valuesCompared = all(map_lgl(
        c('others', 'lowerBounds', 'upperBounds'),
        ~ compareWithinGroup(otherValuesGrouped, thisValuesGrouped, .)
      ))

      if (valuesCompared) {

        if (self$type == "STAT1" && rule$t >= self$t) {
          return(TRUE)
        } else if (self$type == "STAT2" && rule$t <= self$t) {
          return(TRUE)
        } else if (self$type == "STAT3" && min(rule$t) >= min(self$t) && max(rule$t) <= max(self$t)) {
          return(TRUE)
        } else {
          return(FALSE)
        }

      } else {
        return(FALSE)
      }
    },

    #' @description
    #' Method to calculate rule support, certainty, coverage, and strength
    #' @param it the information table to use
    #' @return a named list of integers
    ruleMetrics = function(it) {
      lhs = self$condition$complexCover(it)
      rhs = NA

      classUnions = it$classUnions()

      if (self$type == 'STAT1') {
        rhs = classUnions$upward[self$t, ]
        rhs = it$objects[rhs]
      } else if (self$type == 'STAT2') {
        rhs = classUnions$downward[self$t, ]
        rhs = it$objects[rhs]
      } else if (self$type == "STAT3") {
        classStart = min(self$t)
        classEnd = max(self$t)
        rhs = classUnions$upward[classStart, ] & classUnions$downward[classEnd, ]
        rhs = it$objects[rhs]
      }

      result = list(
        support = length(intersect(lhs, rhs)),
        certainty = length(intersect(lhs, rhs)) / length(lhs),
        coverage = length(intersect(lhs, rhs)) / length(rhs),
        strength = length(intersect(lhs, rhs)) / length(lhs) * (length(lhs) / nrow(it$decisionTable))
      )

      return(result)
    },


    #' @description
    #' print method.
    print = function() {
      cat(self$toString())
      invisible(self)
    },

    #' @description
    #' toString method for the RHS of the rule.
    getRHS_String = function() {
      rhs = NA
      if (self$type == "STAT3") {
        rhs = paste0("Cl", min(self$t), " U ... U ", "Cl", max(self$t))
      } else {
        rhs = paste0("Cl", self$t, ifelse(self$type == 'STAT1', ">=", "<="))
      }

      return(rhs)
    },

    #' @description
    #' toString method.
    toString = function() {
      paste0(self$condition$toString(), " => ", self$getRHS_String())
    },

    #' @description
    #' Method to compare two DecisionRules
    #' @param other the other decision rule to compare to
    #' @return a single boolean value
    equals = function(other) {

      if (any(is.null(other))) {
        return(FALSE)
      }
      stopifnot("DecisionRule" %in% class(other))

      return(self$condition$equals(other$condition) &&
               self$t == other$t &&
               self$type == other$type)
    }
  )
)
