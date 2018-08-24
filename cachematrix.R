#8/24/18 
#These functions cache the inverse of a matrix, so that when 
#it is needed again, the inverse can be pulled the cache instead
#of being recalculated.  This will speed up an otherwise lengthy
##process.


##The makeCacheMatrix function creates a list of functions 
##including: 
##set --set matrix input
##get --get matrix input
##setsolve --set inverse of the matrix
##getsolve --get inverse of the matrix


makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  get <- function() x 
  setsolve <- function(solve) { 
    m <<- solve 
  } 
  getsolve <- function() m 
  
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve) 
} 




##The cacheSolve function checks to see if the inverse
## has been calculated.  If not, it calculates the inverse 
##of the matrix.  If it has been calculated, it retrieves the 
##inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



## Try it out!

## Here is a matrix to test with:
TestMatrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)


#Applying matrix to functions
cacheSolve(makeCacheMatrix(TestMatrix)) 






