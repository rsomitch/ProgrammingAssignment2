## This function creates a special matrix object that can cache its inverse and will
## set the value of the matrix
## get the value of the matrix
## will set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
    
  }
  get <- function() x
  setInverse <- function(Inverse) Inv1 <<- Inverse
  getInverse <- function() Inv1
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. 

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
  Inv1 <- x$getInverse()
  
  ##If the inverse has already been calucated and tth matrix has not changed, 
  ## then the cacheSolve will retrive the inverse from the cache.
  
  if(!is.null(Inv1)){     
    message("getting cached data")
    return(Inv1)
  }
  data <- x$get()
  Inv1 <- solve(data, ...)
  x$setInverse(Inv1)
  Inv1
}

