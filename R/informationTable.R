#' R6 class representing an information table.
#'
#' @description
#' An information table consists of the decision table and meta-data.
#'
#' @details
#' This class stores the decision table, and meta-data.
#' The decision table consists of an object identifier column, a decision column, and at least one additional attribute.
#' The meta-data consist of the attribute names, their types, and the alpha and beta values for similarity attributes.
#'
#' @export
InformationTable <- R6::R6Class(
  classname = "InformationTable",

  public = list(

    #' @field decisionTable the set of examples
    decisionTable = data.frame(),

    #' @field metaData meta-data of the attributes, including their name and type, along with alpha and beta parameters for similarity variables
    metaData = data.frame(),

    ### Derived fields ###
    #' @field objects vector of object names
    objects = NA,

    #' @description
    #' Create a new information table object.
    #' @param decisionTable data frame containing the decision examples
    #' @param metaData data frame containing the meta-data of the attributes. This parameter is optional, and if not provided, we assume all dominance attributes.
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
      objects = decisionTable[[objectColumn]]
      stopifnot(length(unique(objects)) == length(objects))
      self$objects = objects


      decisionColumn = which(metaData$type == 'decision', arr.ind = TRUE)
      stopifnot(length(decisionColumn) == 1)  # we expect exactly one decision column

      stopifnot(metaData %>%
        filter(type == 'similarity') %>%
        filter(is.na(alpha) | is.na(beta)) %>%
        nrow() == 0
      )  # all similarity variables need the alpha and beta parameters provided

      self$metaData = metaData
    },

    #' @description
    #' Method to determine whether another information table is compatible with this one.
    #' @param it the information table to compare to
    isCompatible = function(it) {
      return(class(it) == 'InformationTable' &&
               it$metaData == self$metaData)
    },

    #' @description
    #' Method for calculating the downward class union.
    #' @param class the decision class to compare to
    #' @return the set of objects in the downward class union
    downwardClassUnion = function(class) {

      objectColumn = which(self$metaData$type == 'object', arr.ind = TRUE)
      decisionColumn = which(self$metaData$type == 'decision', arr.ind = TRUE)

      self$decisionTable[, c(objectColumn, decisionColumn)] %>%
        filter(.[[2]] <= class) %>%
        pull(1)
    },

    #' @description
    #' Method for calculating the upward class union.
    #' @param class the decision class to compare to
    #' @return the set of objects in the upward class union
    upwardClassUnion = function(class) {

      objectColumn = which(self$metaData$type == 'object', arr.ind = TRUE)
      decisionColumn = which(self$metaData$type == 'decision', arr.ind = TRUE)

      self$decisionTable[, c(objectColumn, decisionColumn)] %>%
        filter(.[[2]] >= class) %>%
        pull(1)
    },

    #' @description
    #' Method for calculating all downward- and upward class unions at once.
    #' @return a pair of matrices for both class unions, where each row represents a class, and each column represents an object
    classUnions = function() {

      decisionColumn = which(self$metaData$type == 'decision', arr.ind = TRUE)

      decisions = self$decisionTable[[decisionColumn]]
      decisionCard = length(unique(decisions))

      decisionIDs = 1:decisionCard
      decisions = factor(decisions, labels = decisionIDs, levels = sort(unique(decisions)), ordered = TRUE)
      objectCount = length(self$objects)

      upwardClassUnions = matrix(nrow = decisionCard, ncol = objectCount)
      downwardClassUnions = upwardClassUnions

      for (objectID in 1:objectCount) {
        upwardClassUnions[decisionIDs, objectID] = decisions[objectID] >= decisionIDs
        downwardClassUnions[decisionIDs, objectID] = decisions[objectID] <= decisionIDs
      }

      return(list(upward = upwardClassUnions, downward = downwardClassUnions))
    },

    #' @description
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
    #' @param compareSimilaritySwitched whether to test similarity with the parameters switched
    #' @return whether x dominates y on attribute set P
    dominates = function(x, y, P, compareSimilaritySwitched = FALSE) {

      # ERROR-CHECKS:
      stopifnot(length(x) == length(y), length(x) > 0)
      stopifnot(x %in% self$objects, y %in% self$objects)
      stopifnot(P %in% self$metaData$name)

      # the subsets of the decision table, relevant for the object in x and y, respectively:
      X = self$decisionTable[map_int(x, ~ which(. == self$objects)), ] %>% select(P)
      Y = self$decisionTable[map_int(y, ~ which(. == self$objects)), ] %>% select(P)

      # the partitioned set of attributes to consider:
      P = self$partitionAttributes(P)

      R_ind = map_dfc(P$ind, function(q) { X[[q]] == Y[[q]] }) %>% apply(FUN = all, MARGIN = 1)
      if (compareSimilaritySwitched) {
        R_sim = map_dfc(P$sim, ~ self$similar(Y, X, .)) %>% apply(FUN = all, MARGIN = 1)
      } else {
        R_sim = map_dfc(P$sim, ~ self$similar(X, Y, .)) %>% apply(FUN = all, MARGIN = 1)
      }
      R_dom = map_dfc(P$dom, function(q) { X[[q]] >= Y[[q]] }) %>% apply(FUN = all, MARGIN = 1)

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

    #' @description
    #' Method for calculating the P-dominated and P-dominating sets all at once.
    #' @param P the set of attributes to test - vector of attribute names
    dominatingAndDominatedSets = function(P) {

      # ERROR-CHECKS:
      stopifnot(P %in% self$metaData$name)

      # the subsets of the decision table, relevant for the object in x and y, respectively:
      X = self$decisionTable
      Y = self$decisionTable

      # the partitioned set of attributes to consider:
      P = self$partitionAttributes(P)

      # Determine indiscernibility
      n = length(self$objects)
      R_ind = matrix(rep(TRUE, n * n), nrow = n, ncol = n)
      for (i in 1:n) {
        for (j in i:n) {
          result = all(X[i, P$ind] == Y[j, P$ind])
          R_ind[i, j] = result
          R_ind[j, i] = result
        }
      }

      # Determine similarity
      R_sim = matrix(rep(TRUE, n * n), nrow = n, ncol = n)
      simAttributeIndex = map_int(P$sim, ~ which(self$metaData$name == ., arr.ind = T))
      alpha = self$metaData$alpha[simAttributeIndex]
      beta = self$metaData$beta[simAttributeIndex]
      for (i in 1:n) {
        for (j in 1:n) {

          exampleX = X[i, simAttributeIndex]
          exampleY = Y[j, simAttributeIndex]

          R_sim[i, j] = all(abs(exampleX - exampleY) <= alpha * exampleY + beta)
        }
      }
      R_sim_switched = t(R_sim)

      # Determine dominance
      R_dominates = matrix(rep(TRUE, n * n), nrow = n, ncol = n)
      for (i in 1:n) {
        for (j in 1:n) {
          R_dominates[i, j] = all(X[i, P$dom] >= Y[j, P$dom])
        }
      }
      R_dominatedBy = t(R_dominates)

      # Return the results:
      return(list(
        dominating_L = R_ind & R_sim_switched & R_dominatedBy,
        dominating_U = R_ind & R_sim & R_dominatedBy,
        dominated_L = R_ind & R_sim & R_dominates,
        dominated_U = R_ind & R_sim_switched & R_dominates
      ))
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
    },

    #' @description
    #' This method calculates the dominating set of an object with respect to a criterion set.
    #' @param x the object - object name
    #' @param P the criterion set
    #' @param compareSimilaritySwitched whether to test similarity with the parameters switched
    #' @return the set of objects that dominate object x
    dominatingSet = function(x, P, compareSimilaritySwitched = TRUE) {

      stopifnot(x %in% self$objects)

      d = map_lgl(self$objects, ~ self$dominates(., x, P, compareSimilaritySwitched = compareSimilaritySwitched))
      self$objects[d]
    },

    #' @description
    #' This method calculates the dominated set of an object with respect to a criterion set.
    #' @param x the object - object name
    #' @param P the criterion set
    #' @param compareSimilaritySwitched whether to test similarity with the parameters switched
    #' @return the set of objects that are dominated by object x
    dominatedSet = function(x, P, compareSimilaritySwitched = FALSE) {

      stopifnot(x %in% self$objects)

      d = map_lgl(self$objects, ~ self$dominates(x, ., P, compareSimilaritySwitched = compareSimilaritySwitched))
      self$objects[d]
    },

    #' @description
    #' This method calculates the P-upper approximations of the upward class unions.
    #' @param dominating_U the P-dominating sets (U) - matrix
    #' @return the approximations for all classes in a boolean matrix from
    upwardClassUnionUpperApproximation = function(dominating_U) {

      upwardClassUnion = self$classUnions()$upward

      approximations = matrix(nrow = nrow(upwardClassUnion), ncol = ncol(upwardClassUnion))

      for (class in 1:nrow(upwardClassUnion)) {
        result = dominating_U[upwardClassUnion[class,], ]
        approximations[class, ] = apply(result, MARGIN = 2, FUN = any)
      }

      return(approximations)
    },

    #' @description
    #' This method calculates the P-lower approximations of the upward class unions.
    #' @param downward_U the P-upper approximations of the downward class unions - matrix
    #' @return the approximations for all classes in a boolean matrix from
    upwardClassUnionLowerApproximation = function(downward_U) {
      U = rep(TRUE, length(self$objects))

      classCount = nrow(downward_U)
      objectCount = ncol(downward_U)

      approximations = matrix(nrow = classCount, ncol = objectCount)

      approximations[1, ] = U
      approximations[2:classCount, ] = !downward_U[1:(classCount - 1), ]

      return(approximations)
    },

    #' @description
    #' This method calculates the P-upper approximations of the downward class unions.
    #' @param dominated_U the P-dominated sets (U) - matrix
    #' @return the approximations for all classes in a boolean matrix from
    downwardClassUnionUpperApproximation = function(dominated_U) {

      downwardClassUnion = self$classUnions()$downward

      approximations = matrix(nrow = nrow(downwardClassUnion), ncol = ncol(downwardClassUnion))

      for (class in 1:nrow(downwardClassUnion)) {
        result = dominated_U[downwardClassUnion[class,], ]
        approximations[class, ] = apply(result, MARGIN = 2, FUN = any)
      }

      return(approximations)
    },

    #' @description
    #' This method calculates the P-lower approximations of the downward class unions.
    #' @param upward_U the P-upper approximations of the upward class unions - matrix
    #' @return the approximations for all classes in a boolean matrix from
    downwardClassUnionLowerApproximation = function(upward_U) {
      U = rep(TRUE, length(self$objects))

      classCount = nrow(upward_U)
      objectCount = ncol(upward_U)

      approximations = matrix(nrow = classCount, ncol = objectCount)

      approximations[classCount, ] = U
      approximations[1:(classCount - 1), ] = !upward_U[2:classCount, ]

      return(approximations)
    },

    #' @description
    #' This method calculates the P-lower and P-upper approximations of class unions and boundary regions.
    #' @param P the attribute set
    #' @return a named list of the approximations
    roughSets = function(P) {

      dom = self$dominatingAndDominatedSets(P)

      approx = list(
        upward_U = self$upwardClassUnionUpperApproximation(dom$dominating_U),
        upward_L = NA,
        downward_U = self$downwardClassUnionUpperApproximation(dom$dominated_U),
        downward_L = NA
      )

      approx$upward_L = self$upwardClassUnionLowerApproximation(approx$downward_U)
      approx$downward_L = self$downwardClassUnionLowerApproximation(approx$upward_U)

      return(approx)
    },

    #' @description
    #' This method calculates the boundary regions of rough sets.
    #' @param roughSets the class union approximations.
    #' @return the upward and downward boundary regions
    boundaryRegions = function(roughSets) {
      return(list(
        upward = roughSets$upward_U & !roughSets$upward_L,
        downward = roughSets$downward_U & !roughSets$downward_L
      ))
    },

    #' @description
    #' This method calculates the accuracy of the approximations of the class unions.
    #' @param roughSets the approximations
    #' @return a pair of vectors describing the accuracy of the downward and upward class union approximations
    accuracyOfApproximation = function(roughSets) {

      classCount = nrow(roughSets$upward_U)

      acc = list(
        upward = rep(NA_real_, classCount),
        downward = rep(NA_real_, classCount)
      )

      for (class in 1:classCount) {
        acc$upward[class] = sum(roughSets$upward_L[class, ]) / sum(roughSets$upward_U[class, ])
        acc$downward[class] = sum(roughSets$downward_L[class, ]) / sum(roughSets$downward_U[class, ])
      }

      return(acc)
    },

    #' @description
    #' This method calculates the quality of the approximations of the class unions.
    #' @param boundaryRegions the boundary regions of the rough set
    #' @return a number expressing the ratio of all P-correctly sorted actions to all actions in the decision table
    qualityOfApproximation = function(boundaryRegions) {

      incorrectlySorted = apply(boundaryRegions$downward, MARGIN = 2, FUN = any)

      return(sum(!incorrectlySorted) / length(self$objects))
    }
  )
)
