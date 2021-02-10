## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  main <- NULL
  set <- function(y){
    x <<- y
    main <<- NULL
  }
  get <- function() x
  setInverseMain <- function(solveMatrix) main <<- solveMatrix
  setInverseMain <- function() main
  list(set = set, get = get, setInverseMain = setInverseMain, getInverseMain = getInverseMain)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  main <- x$getInverse()
  if(!is.null(main)){
    message("We have a cached data now")
    return(main)
  }
  data <- x$get()
  main <- solve(data)
  x$setInverse(main)
  main      
}
