## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {## There are four functions in this environment, and ALSO the x and inv objects (a consequence of using <<-). And the get and getInverse functions only fetch these from their enclosing environment.
  inv <- NULL ## set inverse flag to null inside functions scope
  set <- function(y) { ## define a function set that sets the inv flag to null
    x <<- y ##the assignment of y to x is valid in parent scope
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inv <<- solve(x) ## set inverse of matrix
  getinverse <- function() inv 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function checks the inv flag to see if the inverse is cached. if its cached it fetches the cached matrix, if not inverse is calculated

cacheSolve <- function(x, ...) {
  ## return the inverse matrix of x
  inv <- x$getinverse()
  if (!is.null(inv)) {
        return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
