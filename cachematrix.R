#This function creates a special "matrix" object that can cache 
#its inverse
##The function takes as argument x, which has as default value 
#an empty matrix.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {                 #sets the value
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse<- function(inverse) m <<-inverse # sets the value, which is not calculated yet.
     getinverse <- function() m  #gets the inverse value
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then cacheSolve
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
  
     m <- x$getinverse() 
     # checks to see if the inverse was already calculated
     if (!is.null(m)) {
          message("getting cached reververse matrix")
          return(m)
     }
     # otherwise calculates the inverse and sets the value with setinverse
     else {
          m <- solve(x$get()) 
          x$setinverse(m)
          return(m)
     }
}
