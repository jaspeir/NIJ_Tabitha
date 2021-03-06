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

      RULES = list(
        STAT1 = c(),
        STAT2 = c(),
        STAT3 = c()
      )

      classCount = nrow(self$roughSets$upward_L)

      # Create STAT1 type rules
      for (t in seq(from = classCount, to = 2)) {
        approx = self$roughSets$upward_L[t, ]
        rules = self$findRules(approximation = approx, P = self$P, t = t, ruleType = "STAT1")

        RULES$STAT1 = self$addMinimalRules(existingRules = RULES$STAT1, newRules = rules)
      }

      # Create STAT2 type rules
      for (t in seq(from = 1, to = classCount - 1)) {
        approx = self$roughSets$downward_L[t, ]
        rules = self$findRules(approximation = approx, P = self$P, t = t, ruleType = "STAT2")

        RULES$STAT2 = self$addMinimalRules(existingRules = RULES$STAT2, newRules = rules)
      }

      # Create STAT3 type rules
      if (classCount >= 2) {
        for (s in seq(from = 1, to = classCount - 1)) {
          for (t in seq(from = s + 1, to = classCount)) {
            approx = self$roughSets$downward_U[s, ] & self$roughSets$upward_U[t, ]
            rules = self$findRules(approximation = approx, P = self$P, t = s:t, ruleType = "STAT3")

            RULES$STAT3 = self$addMinimalRules(existingRules = RULES$STAT3, newRules = rules)
          }
        }
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

      static_examples = self$it
      EXAMPLES = self$it$clone()

      while (length(G) > 0) {

        e = ComplexCondition$new()  # starting complex condition
        S = G  # set of objects currently covered by E

        maxRuleLength = length(P) * length(B)

        while ((e$length() == 0 || !isSubsetArbitrary(e$complexCover(it = EXAMPLES), B)) && e$length() < maxRuleLength) {

          best = NULL

          for (criterion in P) {

            S_index = map_int(S, ~ which(. == EXAMPLES$objects, arr.ind = TRUE))
            values = EXAMPLES$decisionTable[S_index, ][[criterion]]
            for (value in values) {

              # The set of bound types to use when generating a new elementary
              boundTypes = c()
              if (EXAMPLES$getType(criterion) == 'dominance') {
                boundTypes = switch(ruleType,
                  "STAT1" = c(TRUE),
                  "STAT2" = c(FALSE),
                  "STAT3" = c(TRUE, FALSE),
                )
              } else {
                boundTypes = c(NA)
              }

              for (boundType in boundTypes) {
                check = ElementaryCondition$new(attribute = criterion, value = value, it = EXAMPLES, isLowerBound = boundType)
                best = e$findBestElementary(G = G, it = EXAMPLES, check = check, best = best)
              }
            }

          }

          # Detect endless rules being generated and finish creating the complex:
          if (e$contains(best)) {
            break
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
      rules = map(E, ~ DecisionRule$new(condition = ., t = self$it$decodeDecisions(t), type = ruleType))

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
      allRules = c(self$rules$STAT1, self$rules$STAT2, self$rules$STAT3)
      paste(map_chr(allRules, function(r) r$toString()), collapse = '\n')
    }
  )
)
