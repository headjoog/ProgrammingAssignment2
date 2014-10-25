## cachematrix.R - two functions for Coursera R Programming Assignment 2.
##                 See details below.
##   author:  muttinthehut (JoeB615), 2014-10-20
##   

## makeCacheMatrix:  creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #  get: get the value of the matrix
    get <- function() x
    
    # set:  set the value of the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # setinverse: set the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    
    # getinverse: get the inverse of the matrix
    getinverse <- function() m
    
    # return the results
    list(set = set, get = get
        ,setinverse=setinverse
        ,getinverse=getinverse)    

}


## cacheSolve:  computes the inverse of the special "matrix"
##              returned by "makeCacheMatrix".  If the inverse
##              has already been calculated (and the matrix hasn't)
##              changed), the retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       ## get the inverse of the matrix
       m <- x$getinverse()
       
       ## check if there's alreay a matrix
      if (!is.null(m)) {
        message("grabbing data from the cache")
        return(m)
      }
      
      ## if not:  get the inverse of the matrix
      data <- x$get()
      m <- solve(data, ...)
      ## set the inverse of the matrix
      x$setinverse(m)
      m
}
