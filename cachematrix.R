# the following functions work in conjunction to calculate the inverse 
# of a matrix, assuming the passed matrix is invertible, using R caching 
# abilities to avoid doing unnecessary computations

# makeCacheMatrix function will take a matrix as a argument and return
# a "special matrix", that is an object that contains a matrix and a set
# of functions that operate on the contained matrix. The functions are:
# set() which set the matrix variable
# get() which get the stored matrix variable
# set_inverse() which set the matrix inverse variable
# and get_inverse() which return the stored matrix inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
    i <- NULL
    set <- function(y) {
      mtrx <<- y
      i <<- NULL
    }
    
    get <- function() mtrx
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# cacheSolve will take a "special matrix" and return the inverse of its
# matrix, if the special matrix already contains the inverse of its matrix, 
# this function will not recompute the inverse, instead it will return
# the cached result.

cacheSolve <- function(mtrx, ...) {
    i <- mtrx$get_inverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- mtrx$get()
    i <- solve(data, ...)
    mtrx$set_inverse(i)
    i
}
