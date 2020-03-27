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
      self$main()
    },

    #' @description
    #' Main method.
    main = function() {
      RULES = c()

      # TODO: make sure we loop over the rough sets in the right order, and skip any unnecessary elements

      # Create STAT1 type rules
      for (approx in self$roughSets$upward_L) {
        rules = findRules(approx, self$P, ruleType = "STAT1")

        # TODO: make rules from the extracted complex conditions

        RULES = c(RULES, rules)
      }

      # Create STAT2 type rules
      for (approx in self$roughSets$downward_L) {
        rules = findRules(approx, self$P, ruleType = "STAT2")
        RULES = c(RULES, rules)
      }

      # TODO: save RULES

    },

    #' @description
    #' Method to extract a given type of decision rules for a given class union approximation.
    #' @param approximation the class union approximation to cover
    #' @param P the set of attributes used for creating the approximations
    #' @param ruleType the type of rule to extract
    #' @return the extracted decision rules
    findRules = function(approximation, P, ruleType) {

      B = approximation
      G = B    # objects still to cover
      E = c()  # the set of extracted rules

      isLowerBound = ruleType %in% c(1, 3, "one", "three", "STAT1", "stat1") # Type of rule to generate (for dominance variables)

      static_examples = self$it
      EXAMPLES = self$it$copy()

      while (length(G) > 0) {

        e = ComplexCondition$new(c())  # starting complex condition
        S = G  # set of objects currently covered by E

        while (e$length() == 0 || !isSubset(e$complexCover(it = EXAMPLES), B)) {

          best = NA

          for (criterion in P) {

            values = EXAMPLES$decisionTable[S, criterion]
            for (value in values) {
              check = ElementaryCondition$new(attribute = criterion, value = value, it = EXAMPLES, isLowerBound = isLowerBound)
              best = e$findBestElementary(G = G, it = EXAMPLES, check = check, best = best)
            }

          }

          e.append(best)
          covered = best$elementCover(it = EXAMPLES)
          S = S & covered
        }

        e = e$checkRules(B = B, it = static_examples)
        E = c(E, e)

        # remove examples covered by RULE
        EXAMPLES = EXAMPLES$remove_objects(self$rules_cover(it = EXAMPLES, rules = E))

        coveredGlobal = self$rules_cover(it = static_examples, rules = E)
        G = B & !coveredGlobal
      }

      return(E)
    },

    #' @description
    #' Method to calculate the objects covered by a set of rules.
    #' @param it the infromation table to work on
    #' @param rules the set of rules
    #' @return the covered objects - boolean vector
    rulesCover = function(it, rules) {
      covered = map_dfr(rules, function(rule) { rule$complexCover(it) })
      return(apply(covered, MARGIN = 2, FUN = any))
    }
  )
)
