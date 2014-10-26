?## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## Returning the entered matrix
      get <- function() x
      
      ## Setting the inverse of the matrix
      setinverse <- function(solve) m <<- solve
      
      ## Getting the inverse of the matrix
      getinverse <- function() m
      
      ##Creating a list
      list(set=set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      
      ##Check if a matrix pre-exists in cache
      if(!is.null(m)) {
            message("Getting Cached Matrix")
            return(m)
      }
      
      ## If a matrix doesn't pre-exist in the cache, calculate the inverse of a new one
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
          
}
      
      
