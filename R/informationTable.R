#' A class for storing all relevant information of an information table.
#'
#' It stores the decision table, the types of the attributes, and the alpha and beta values of the similarity attributes.
#'
#' @export
InformationTable <- R6::R6Class(
  classname = "InformationTable",

  public = list(

    # the set of examples
    decisionTable = data.frame(),

    # meta-data of the attributes, including their name and type,
    # along with alpha and beta parameters for similarity variables
    metaData = data.frame(),

    ### Derived fields ###
    # vector of object names
    objects = NA,


    #' Constructor. The meta-data dataframe is optional, and if not provided, we assume all dominance attributes.
    initialize = function(decisionTable, metaData = NA) {

      # ERROR-CHECKS on the decision table:
      stopifnot('data.frame' %in% class(decisionTable))
      stopifnot(ncol(decisionTable) >= 3)  # at least one attribute apart from object and decision
      self$decisionTable = decisionTable

      # ERROR-CHECKS on the meta-data:
      if (all(is.na(metaData))) {

        # if meta-data not provided, then set the types as follows:
        # - first attribute to object,
        # - last attribute to decision,
        # - all other attributes to dominance

        attributeCount = ncol(decisionTable)

        metaData = data.frame(
          name = names(decisionTable),
          type = c('object', rep('dominance', attributeCount - 2), 'decision'),
          alpha = rep(NA_real_, attributeCount),
          beta = rep(NA_real_, attributeCount)
        )
      }

      stopifnot('data.frame' %in% class(metaData))
      stopifnot(setequal(c('name', 'type', 'alpha', 'beta'), names(metaData)))  # it should contain exactly the name, type, alpha and beta columns
      stopifnot(setequal(names(decisionTable), metaData$name))  # need meta-data for all columns of the decision table

      metaData$type = factor(metaData$type,
                          levels = c('indiscernibility', 'similarity', 'dominance', 'misc', 'object', 'decision'),
                          ordered = FALSE)

      objectColumn = which(metaData$type == 'object', arr.ind = TRUE)
      stopifnot(length(objectColumn) == 1)  # we expect exactly one object column
      self$objects = decisionTable[[objectColumn]]

      stopifnot(metaData %>%
        filter(type == 'similarity') %>%
        filter(is.na(alpha) | is.na(beta)) %>%
        nrow() == 0
      )  # all similarity variables need the alpha and beta parameters provided

      self$metaData = metaData
    },

    #' Method to determine whether another information table is compatible with this one.
    isCompatible = function(it) {
      return(class(it) == 'InformationTable' &&
               it$metaData == self$metaData)
    },

    #' Method that partitions attribute set P into into sets of the same attribute type.
    #' Only types relevant for the dominance relation are considered (indiscernibility, similarity, and dominance).
    #' @param P the set of attributes to partition - vector of attribute names
    #' @return a list of attribute sets
    partitionAttributes = function(P) {

      types = self$metaData %>%
        filter(name %in% P) %>%
        select(name, type)

      P_ind = types %>% filter(type == "indiscernibility") %>% pull(name)
      P_sim = types %>% filter(type == "similarity") %>% pull(name)
      P_dom = types %>% filter(type == "dominance") %>% pull(name)

      return(list(ind = P_ind, sim = P_sim, dom = P_dom))
    },

    #' Function to determine whether x dominates y on the mixed attribute set P.
    #' @param x the left operand - object name
    #' @param y the right operand - object name
    #' @param P the set of attributes to test - vector of attribute names
    #' @return whether x dominates y on attribute set P
    dominates = function(x, y, P) {

      # ERROR-CHECKS:
      stopifnot(length(x) == length(y), length(x) > 0)
      stopifnot(x %in% self$objects, y %in% self$objects)
      stopifnot(P %in% self$metaData$name)

      # the subsets of the decision table, relevant for the object in x and y, respectively:
      X = self$decisionTable[map_int(x, ~ which(. == self$objects)), P]
      Y = self$decisionTable[map_int(y, ~ which(. == self$objects)), P]

      # the partitioned set of attributes to consider:
      P = self$partitionAttributes(P)

      R_ind = map_dfc(P$ind, function(q) {pull(X, !!q) == pull(Y, !!q)}) %>% apply(FUN = all, MARGIN = 1)
      R_sim = map_dfc(P$sim, ~ self$similar(X, Y, .)) %>% apply(FUN = all, MARGIN = 1)
      R_dom = map_dfc(P$dom, ~ outranks(X, Y, .)) %>% apply(FUN = all, MARGIN = 1)

      if (length(R_ind) == 0) {
        R_ind = rep(TRUE, nrow(X))
      }
      if (length(R_sim) == 0) {
        R_sim = rep(TRUE, nrow(X))
      }
      if (length(R_dom) == 0) {
        R_dom = rep(TRUE, nrow(X))
      }

      R_ind & R_sim & R_dom
    },

    #' Method to determine whether x is similar to y on attribute q.
    #' @param x the left operand - a data frame
    #' @param y the right operand - a data frame
    #' @param q the attribute to test
    #' @return whether x is similar to y on attribute q
    similar = function(x, y, q) {

      # ERROR-CHECKS:
      stopifnot(nrow(x) == nrow(y), nrow(x) > 0, q %in% names(x), q %in% names(y))

      exampleX = x[[q]]
      exampleY = y[[q]]

      attributeIndex = which(self$metaData$name == q)
      alpha = self$metaData$alpha[attributeIndex]
      beta = self$metaData$beta[attributeIndex]

      abs(exampleX - exampleY) <= alpha * exampleY + beta
    }

  )
)
