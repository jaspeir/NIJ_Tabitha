#' R6 class representing the DOMLEM algorithm for mixed-type attributes.
#'
#' @description
#' The DOMLEM algorithm extracts a minimal set decision rules from an information table.
#'
#' @details
#' This class stores methods used in extracting the decision rules.
#' This is an extension of the original DOMLEM algorithm to consider mixed-type attributes.
#'
#' @export
DOMLEM <- R6::R6Class(
  classname = "DOMLEM",

  public = list(

    #' @field it the information table to extract the decision rules from
    it = NA,

    #' @field P the P-attributes
    P = NA,

    ### Extracted fields ###
    #' @field roughSets the P-lower and P-upper approximations of the class unions
    roughSets = NA,

    ### Calculated fields ###
    #' @field rules the extracted decision rules
    rules = NA,

    #' @description
    #' Create a new DOMLEM object.
    #' @param it the infromation table to work on
    #' @param P the set of attributes to create the rough sets
    initialize = function(it, P) {

      # ERROR-CHECKS on the information table parameter:
      stopifnot('InformationTable' %in% class(it))
      self$it = it

      self$roughSets = it$roughSets(P)
      self$P = P

      # Run main method
      # self$main()
    },

    #' @description
    #' Main method.
    main = function() {
      RULES = c()

      classCount = nrow(self$roughSets$upward_L)

      # Create STAT1 type rules
      for (t in seq(from = classCount, to = 2)) {
        approx = self$roughSets$upward_L[t, ]
        rules = self$findRules(approximation = approx, P = self$P, t = t, ruleType = "STAT1")

        RULES = self$addMinimalRules(existingRules = RULES, newRules = rules)
      }

      # Create STAT2 type rules
      for (t in seq(from = 1, to = classCount - 1)) {
        approx = self$roughSets$downward_L[t, ]
        rules = self$findRules(approximation = approx, P = self$P, t = t, ruleType = "STAT2")

        RULES = self$addMinimalRules(existingRules = RULES, newRules = rules)
      }

      self$rules = RULES
    },

    #' @description
    #' Method to extract a given type of decision rules for a given class union approximation.
    #' @param approximation the class union approximation to cover
    #' @param P the set of attributes used for creating the approximations
    #' @param t the t-parameter of the class union
    #' @param ruleType the type of rule to extract
    #' @return the extracted decision rules
    findRules = function(approximation, P, t, ruleType) {

      B = self$it$objects[approximation]
      G = B    # objects still to cover
      E = c()  # the set of extracted rules

      isLowerBound = ruleType %in% c(1, 3, "one", "three", "STAT1", "stat1") # Type of rule to generate (for dominance variables)

      static_examples = self$it
      EXAMPLES = self$it$clone()

      while (length(G) > 0) {

        e = ComplexCondition$new()  # starting complex condition
        S = G  # set of objects currently covered by E

        while (e$length() == 0 || !isSubsetArbitrary(e$complexCover(it = EXAMPLES), B)) {

          best = NULL

          for (criterion in P) {

            S_index = map_int(S, ~ which(. == EXAMPLES$objects, arr.ind = TRUE))
            values = EXAMPLES$decisionTable[S_index, ][[criterion]]
            for (value in values) {
              check = ElementaryCondition$new(attribute = criterion, value = value, it = EXAMPLES, isLowerBound = isLowerBound)
              best = e$findBestElementary(G = G, it = EXAMPLES, check = check, best = best)
            }

          }

          e$append(best)
          covered = best$elementCover(it = EXAMPLES)
          S = intersect(S,covered)
        }

        e = e$reduceConditions(B = B, it = static_examples)
        E = c(E, e)

        # remove examples covered by E
        EXAMPLES = EXAMPLES$removeObjects(self$rulesCover(it = EXAMPLES, rules = E))

        coveredGlobal = self$rulesCover(it = static_examples, rules = E)
        G = setdiff(B, coveredGlobal)
      }

      # Make rules from the extracted complex conditions:
      classUnionType = switch(ruleType, "STAT1" = 'upward', "STAT2" = 'downward')
      rules = map(E, ~ DecisionRule$new(condition = ., t = t, type = classUnionType))

      return(rules)
    },

    #' @description
    #' Method to calculate the objects covered by a set of rules.
    #' @param it the infromation table to work on
    #' @param rules the set of rules
    #' @return the covered objects - set of object names
    rulesCover = function(it, rules) {

      covered = character(0)
      walk(rules, function(rule) {
        covered <<- union(covered, rule$complexCover(it))
      })

      return(covered)
    },

    #' @description
    #' Method to add the minimal decision rules to the existing set of rules.
    #' @param existingRules the set of rules to be extended
    #' @param newRules the set of rules to be added
    #' @return a set of decision rules
    addMinimalRules = function(existingRules, newRules) {
      for (rule in newRules) {
        if (rule$isMinimal(it = self$it, rules = existingRules)) {
          existingRules = c(existingRules, rule)
        }
      }

      return(existingRules)
    }
  )
)
