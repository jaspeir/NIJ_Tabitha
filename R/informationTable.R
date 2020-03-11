#' A class for storing all relevant information of an information table.
#'
#' It stores the decision table, the types of the attributes, and the alpha and beta values of the similarity attributes.
#'
#' @export
InformationTable <- R6::R6Class(
  "InformationTable",
  public = list(
    decisionTable = data.frame(),
    types = NA,
    alpha = NA,
    beta = NA,

    initialize = function(decisionTable, types = NA, alpha = NA, beta = NA) {
      stopifnot('data.frame' %in% class(decisionTable))

      self$decisionTable = decisionTable
      if (all(is.na(types))) {
        types = rep('indiscernibility', ncol(decisionTable))
      }
      self$types = factor(types,
                     levels = c('indiscernibility', 'similarity', 'dominance', 'misc', 'object', 'decision'),
                     ordered = FALSE)

      isSimilarity = types == 'similarity'
      if (any(isSimilarity)) {
        stopifnot(length(alpha) == ncol(decisionTable), length(beta) == ncol(decisionTable))
        stopifnot(all(!is.na(alpha[isSimilarity])), all(!is.na(beta[isSimilarity])))
        self$alpha = alpha
        self$beta = beta
      } else {
        self$alpha = rep(NA_real_, ncol(decisionTable))
        self$beta = rep(NA_real_, ncol(decisionTable))
      }
    }
  )
)
