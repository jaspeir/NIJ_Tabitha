#' R6 class representing an elementary condition.
#'
#' @description
#' An elementary condition is a filter condition on a single attribute.
#'
#' @details
#' This class stores the attribute, the filter value, and for dominance variables whether the value is a lower- or upper bound.
#'
#' @export
ElementaryCondition <- R6::R6Class(
  classname = "ElementaryCondition",

  public = list(

    #' @field attribute the name of the attribute we filter on
    attribute = NA,
    #' @field value the constant in the filter condition
    value = NA,
    #' @field isLowerBound for dominance attributes whether the filter condition is a lower-bound or upper-bound
    isLowerBound = NA,
    #' @field attributeType the type of the attribute (ind, sim, or dom)
    attributeType = NA,
    #' @field alpha for similarity attributes the alpha parameter
    alpha = NA,
    #' @field beta for similarity attributes the beta parameter
    beta = NA,

    #' @description
    #' Create a new ElementaryCondition object.
    #' @param attribute the attribute we filter on
    #' @param value the value we use in the filter condition
    #' @param it the information table to use
    #' @param isLowerBound [optional] for dominance operators, whether the filter condition is a lower- or upper bound
    initialize = function(attribute, value, it, isLowerBound = NA) {

      # ERROR-CHECKS:
      stopifnot('character' %in% class(attribute))
      stopifnot('InformationTable' %in% class(it))
      stopifnot('logical' %in% class(isLowerBound))
      stopifnot(length(attribute) == 1)
      stopifnot(length(value) == 1)
      stopifnot(length(isLowerBound) == 1)

      self$attribute = attribute
      self$value = value
      self$isLowerBound = isLowerBound

      metaData = it$metaData[it$metaData$name == attribute, ]
      self$attributeType = metaData$type
      self$alpha = metaData$alpha
      self$beta = metaData$beta
    },

    #' @description
    #' Method to calculate the set of objects matching this elementary condition.
    #' @param it the information table to use
    #' @return the set of matching objects - set of object names
    elementCover = function(it) {

      values = it$decisionTable[[self$attribute]]

      covered = switch(as.character(self$attributeType),
              'indiscernibility' = values == self$value,
              'similarity' =  abs(values - self$value) <= self$alpha * self$value + self$beta,
              'dominance' = if (self$isLowerBound) values >= self$value else values <= self$value
      )

      return(it$objects[covered])
    },

    #' @description
    #' Print method.
    print = function() {
      cat(self$toString())

      invisible(self)
    },

    #' @description
    #' toString method.
    toString = function() {
      op = switch(as.character(self$attributeType),
                  'indiscernibility' = '=',
                  'similarity' =  '~',
                  'dominance' = if (self$isLowerBound) '>=' else '<='
      )
      return(paste0(self$attribute, ' ', op, ' ', self$value))
    },

    #' @description
    #' Method to compare two ElementaryConditions.
    #' @param other the other elementary condition to compare to
    #' @return a single boolean value
    equals = function(other) {

      if (any(is.null(other))) {
        return(FALSE)
      }
      stopifnot("ElementaryCondition" %in% class(other))

      return(
        self$attribute == other$attribute &&
        self$value == other$value &&
        (is.na(self$isLowerBound) && is.na(other$isLowerBound) || self$isLowerBound == other$isLowerBound)
      )
    }
  )
)
