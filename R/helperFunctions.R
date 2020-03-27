#' Function that filters a matrix and returns an object list
#' @param m a boolean matrix
#' @param objects the vector of object names
#' @return a list of vectors
#' @export
convertMatrixToList = function(m, objects) {

  map(1:nrow(m), function(r) { objects[m[r, ]] })
}

#' Function to determine whether A is a subset of B.
#' @param A the left operand - vector
#' @param B the right operand - vector
#' @return the boolean value of the operation
isSubset = function(A, B) {
  stopifnot(length(A) == length(B))

  return(A & B == A)
}
