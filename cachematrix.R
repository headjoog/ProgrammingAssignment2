## cachematrix.R - two functions for Coursera R Programming Assignment 2.
##                 See details below.
##   author:  muttinthehut (JoeB615), 2014-10-20
##   

## makeCacheMatrix:  creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # variable "m" stores the matrix
    m <- NULL
    
    #  get: return the value of the matrix (in the current environment)
    get <- function() x
    
    # set:  set the value of the matrix to the parameter
    #       and copy to variable "x".  
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    # setinverse: set the inverse of the matrix using "solve" function.
    #             Note the "superassignment" to m, which is found in 
    #             calling environments.
    setinverse <- function(solve) m <<- solve
    
    # getinverse: get the inverse of the matrix
    getinverse <- function() m
    
    # construct a list of functions that have been defined
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
       
       ## check if there's alreay a matrix.  If it's defined
       ## return the cached value (let the user know it's cached)
       ## and exit function
      if (!is.null(m)) {
        message("grabbing data from the cache")
        return(m)
      }
      
      ## if we haven't exited function (from using cached version above)
      ## assign the matrix to a variable...
      data <- x$get()
      
      ## and use the solve function to get the inverse
      m <- solve(data, ...)
      
      ## set the inverse of the matrix
      x$setinverse(m)
      
      ## and return value
      m
}
