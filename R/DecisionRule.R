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

    #' @field t the right-hand side of the implication - t-parameter of class union
    t = NA,

    #' @field type the right-hand side of the implication - a class union type (upward or downward)
    type = NA,

    #' @description
    #' Create a new DecisionRule object.
    #' @param condition the complex condition
    #' @param t the t-parameter of the class union
    #' @param type the type of the class union
    initialize = function(condition, t, type) {

      # ERROR-CHECKS:
      stopifnot('ComplexCondition' %in% class(condition))
      stopifnot('numeric' %in% class(t))
      stopifnot(length(type) == 1, type %in% c('upward', 'downward'))

      self$condition = condition
      self$t = t
      self$type = type
    }
  )
)
