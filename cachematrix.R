## Put comments here that give an overall description of what your
## functions do

# function makeCacheMatrix takes in a
# knowingly invertible matrix and returns a
# special 'matrix' that can be processed by
# the function cacheSolve

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  # creates the 'special matrix of functions and
  # stored variables (matrix, and cached inversion)
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# resolves the 'special' matrix created in 
# makeCacheMatrix by checking to see if the 
# inversion matrix has already been solved for
# and creating the inversion matrix if it hasn't
# been solved for

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  # checks for cached inversion matrix
  # 'null' if not cached
  # m = inversion matrix if cached
  
  m <- x$getinverse()
  if(!is.null(m)) {
    
    message('getting cached data')
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
}
