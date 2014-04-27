
# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  # Variables created inside a function exist only for the life time of a function. 
  # Thus, they are not accessible outside of the function. 
  # To force variables in functions to exist globally and behave as a 'cache' we use this special assignment operator: '<<-'. 
  # If a global variable is used in a function, then the global variable will be masked only within the function.
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setInverse  <- function(inverse) i  <<- inverse
  getInverse  <- function() inverse
  list(set= set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

# Computes the inverse of the matrix x and stores it in a global cache. 
cacheSolve <- function(x, ...) {
  
  inverse  <- x$getInverse()
  # If the inverse has already been calculated then the cacheSolve retrieve the inverse from the cache.
  if (!is.null(inverse)){
    # The inverse has already been calculated.
    message("Getting inverse matrix cached data")
    return(inverse)
  }
  data  <- x$get()
  # The inverse hasn't  been calculated yet.
  ## solve() returns the inverse.of the matrix
  inverse  <- solve(data)
  # We store it in the cache and return as the function result.
  x$setInverse(inverse)
  # Return the inverse matrix x
  return(inverse)
}
