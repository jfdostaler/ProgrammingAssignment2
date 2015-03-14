## cacheMatrix.R
## author: JF Dostaler
##
## Creates an object that contains a matrix, and can cache its inverse (an expensive calculation)


## makeCacheMatrix: Creates the matrix with inverse-caching capabilities
##    param x: the original matrix (defaults to an empty matrix)

makeCacheMatrix <- function(x = matrix()) {
  # initialize the variable that will store the cached inverse
  i <- NULL

  # Define a set function
  # The `<<-` operator searches parent environments, and finds the appropriate variable to set
  # For information on the operator:
  #    ?`<<-`
  set <- function(y) {
    # sets the x defined as a parameter to the makeCacheMatrix function
    x <<- y
    
    # sets the i defined above (cache of the inverse)
    i <<- NULL
  }
  
  # function to return the original matrix
  get <- function() x
  
  # function to set the inverse (cached value)
  setinverse <- function(inverse) i <<- inverse

  # function to return the cached inverse
  getinverse <- function() i
  
  # expose the functions we just defined as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Solves for the matrix inverse and caches the value
##    param x: a matrix, or an object (list) returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # make sure we have the correct datatype
  if (class(x) == 'matrix') {
    # if x is a plain matrix, convert it using makeCacheMatrix
    cacheMatrix <- makeCacheMatrix(x)
  } else if (class(x) == 'list' && any(names(x) == 'getinverse')) {
    # this looks like a cacheMatrix already 
    cacheMatrix <- x
  } else {
    # not a valid parameter
    stop("Can't solve for this object (not a valid matrix)")
  }
  
  # if the value is cached, just return early
  i <- cacheMatrix$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if we get this, we need to solve for the inverse and cache it
  data <- cacheMatrix$get()
  i <- solve(data, ...)
  cacheMatrix$setinverse(i)
  
  # return the result  
  i
}
