## testCacheMatrix.R
## author: JF Dostaler
##
## Some test methods for code in cachematrix.R

source('cachematrix.R')

## sampleMatrix: Creates a matrix by randomly sampling the integers from 1 to n*n
##    param n: The matrix dimension. Will generate an n x n square matrix
sampleMatrix <- function(n = 5) {
  makeCacheMatrix( matrix( sample(n*n), ncol=n, nrow=n ) )
}

## check: Verifies that the generated inverse is correct
##    param cacheMatrix: The matrix to verify
##    param tol: The acceptable amount of floating-point precision error
check <- function(cacheMatrix, tol=10e-12) {
  # Solve for the inverse (unless of course it's already cached)
  cacheSolve(cacheMatrix)
  
  # assign to local variables to make this easier to read
  m <- cacheMatrix$get()
  i <- cacheMatrix$getinverse()
  
  # For a matrix M with an inverse N:
  #   M * N = I
  # where I is the identity matrix of the same dimensions as M and N.
  
  # Since it's not reliable to test exact equality on floating-point numbers, we'll
  # calculate how far off the result is from the identity matrix.
  #   m %*% i : Does matrix multiplication
  #   diag(n) : Generates an n x n identity matrix
  diff <- m %*% i - diag(ncol(m))

  # Count the elements that are outside of our tolerance value
  errs <- sum( abs(diff) > tol ) 
  
  # Print a message to the console with the result
  if (errs > 0) {
    warning(sprintf('Found %d values not within tolerances of %e.', errs, tol) )
  } else {
    message('The matrix multiplied with its inverse is the identity, within tolerances.')
  }
}