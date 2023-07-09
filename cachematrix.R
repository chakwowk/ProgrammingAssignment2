## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## defind the argument with default mode of "matrix
        inv <- NULL                         ## inv as NULL, will hold the inverse "matrix
        set <- function(y) {                ## defind the set function to assign new.
             x <<- y                        ## value of the matrix in parent environment.
             inv <<- NULL                   ## if new matrix, reset inv to NULL.
        }
        get <- function() x                 ## get function returns the value of the matrix argument.
        setinverse <- function(inverse)
                inv <<- inverse             ## set value of inverse in parent environment.
        getinverse <- function()inv         ## get value of inverse when called.
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
        

}


## This function computes the inverse of the special matrix "makeCacheMatrix" above,if the inverse has already been calculated,
##it returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        if(!is.NULL(inv)) {
         message("getting cached data")
         return(inv) 
        }
        data <- x$get()
        inv <- solveCache(data, ...)
        x$setinverse(inv)
        inv
}
