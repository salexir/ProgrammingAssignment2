## These functions allow for caching of calculation results that are CPU-intensive.
## If the function has been ran at least once, the value is stored in cache. Else, it 
#  is calculated and added to the cache list via getters. Setters also allow for values to 
# change, if an update is needed.

#This function allows for modifying(via setMatrix, setInverse) as well as reading values input into the matrix cache.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setMatrix <-function(y){
      x <<- y
      inv <<- NULL
    }
    getMatrix <- function() x
    
    setInverse <-function(inverse) inv <<-inverse #append inv value to return value from fnc.
    
    getInverse <-function() inv
    
    list(getMatrix = getMatrix, getInverse = getInverse, setMatrix = setMatrix, setInverse = setInverse)
  }

## This function will check whether the inverse operation has been performed previously. 
#If yes, it will fetch the value and display a message to user.Else, it calculates the value
#and places in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  newdat <- x$getMatrix()
  inv <- solve(newdat, ...)
  x$setInverse(inv)
  inv
}
