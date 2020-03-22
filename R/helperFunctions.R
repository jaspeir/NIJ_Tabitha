#' Function that filters a matrix and returns an object list
#' @param m a boolean matrix
#' @param objects the vector of object names
#' @return a list of vectors
#' @export
convertMatrixToList = function(m, objects) {

  map(1:nrow(m), function(r) { objects[m[r, ]] })
}
