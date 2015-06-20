## The following functions compute and cache the inverse matrix
##

# makeCacheMatrix: 
## This function creates a "matrix" object that can cache its inverse.

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(m = matrix()) {
  inverseM <- NULL
  set <- function(x) {
    m <<- x;
    inverseM <<- NULL;
  }
  get <- function() m;
  setinv <- function(inv) inverseM <<- inv;
  getinv <- function() inverseM;
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function return the Inverse Matrix (Set bt makeCacheMatrix function)
## If the inverse has been computed, this function retrieve the Inverse Matrix from Cache
## If not, the function compute it and store in the Cache.

cacheSolve <- function(m, ...) {
  # Retrieve the Inverse Matrix from the list (try)
  inverseM <- m$getinv()
  
  # Return the Inverse Matrix from Cache if Available
  if(!is.null(inverseM)) {
    # Print Message to Indicate the Inverse Matrix is from Cache
    message("Getting cached data...")
    return(inverseM)
  }
  
  # Compute the Inverse Matrix if It Is Not In Cache
  data <- m$get()
  inverseM <- solve(data, ...)
  
  #Store the Inverse Matrix in Cache
  m$setinv(inverseM)
  
  #Return the Inverse Matrix
  inverseM
}