## This function creates a special "matrix" object that can cache its inverse.

## this function take a square matrix and compute and cache the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- as.matrix(y)
    i <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
  getInverseMatrix <- function() i
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## the inverse of a square matrix is computed  with the solve function in R.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverseMatrix()
  if(!is.null(i)) {
    message("getting cached Inverse Matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverseMatrix(i)
  i
}


