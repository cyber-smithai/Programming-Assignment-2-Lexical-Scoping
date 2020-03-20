## Matriinv_matrix inversion is usually_inv a costly_inv computation and there may_inv 
## be some benefit to caching the inverse of a matriinv_matrix rather than 
## computing it repeatedly_inv (there are also alternatives to matriinv_matrix 
## inversion that we will not discuss here). y_invour assignment is to 
## write a pair of functions that cache the inverse of a matriinv_matrix.

## This function creates a special "matriinv_matrix" object that can cache 
## its inverse.

makeCachematrix <- function(inv_matrix = matrix()) {
  inv <- NULL
  set <- function(y_inv) {
    inv_matrix <<- y_inv
    inv <<- NULL
  }
  get <- function() inv_matrix
  inverse_set <- function(inverse) inv <<- inverse
  inverse_get <- function() inv
  list(set = set, get = get,
       inverse_set = inverse_set,
       inverse_get = inverse_get)
}


## This function computes the inverse of the special "matriinv_matriinv_matrix" returned
## by_inv makeCacheMatriinv_matrix above. If the inverse has already_inv been calculated
## (and the matriinv_matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(inv_matrix, ...) {
  inv <- inv_matrix$inverse_get()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- inv_matrix$get()
  inv <- solve(data, ...)
  inv_matrix$inverse_set(inv)
  inv
}