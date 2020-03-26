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

    ### Calculated fields ###
    #' @field covered the set of matching objects - logical vector for all objects
    covered = NA,

    #' @description
    #' Create a new ElementaryCondition object.
    #' @param attribute the attribute we filter on
    #' @param value the value we use in the filter condition
    #' @param it the information table to use
    #' @param isLowerBound [optional] for dominance operators, whether the filter condition is a lower- or upper bound
    initialize = function(attribute, value, it, isLowerBound = NA) {

      # ERROR-CHECKS:
      stopifnot('character' %in% class(attribute))
      stopifnot('numeric' %in% class(value))
      stopifnot('InformationTable' %in% class(it))
      stopifnot('logical' %in% class(isLowerBound))
      self$attribute = attribute
      self$value = value

      metaData = it$metaData[it$metaData$name == attribute, ]
      self$attributeType = metaData$type
      self$alpha = metaData$alpha
      self$beta = metaData$beta

      self$covered = self$elementCover(it)
    },

    #' @description
    #' Method to calculate the set of objects matching this elementary condition.
    #' @param it the information table to use
    #' @return the set of matching objects - logical vector
    elementCover = function(it) {

      values = it$decisionTable[[self$attribute]]

      cover = switch(self$attributeType,
              'indiscernibility' = values == self$value,
              'similarity' =  abs(values - value) <= self$alpha * value + self$beta,
              'dominance' = if (isLowerBound) values >= self$value else values <= self$value
      )

      return(cover)
    },

    #' @description
    #' Print method.
    print = function() {
      op = switch(self$attributeType,
                     'indiscernibility' = '=',
                     'similarity' =  '~',
                     'dominance' = if (isLowerBound) '>=' else '<='
      )
      cat(paste0(self$attribute, ' ', op, ' ', self$value))

      invisible(self)
    }
  )
)
