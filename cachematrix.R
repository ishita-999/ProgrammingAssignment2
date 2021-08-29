
## creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }



## cacheSolve(): computes the inverse of the “matrix” returned by above makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## This Returns a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
  }
