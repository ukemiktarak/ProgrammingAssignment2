## COURSERA R Programming
## Programming Assignment 2
## By: Umut Kemiktarak 
## Username: ukemiktarak

## This function creates a special "matrix" 
##   object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## init inverse cache
    inv <- NULL
    
    ## setters and getters
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## list of public functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special 
##   "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if it is already cached
    i <- x$getinverse()
    
    ## return from cache if it is already set
    if(!is.null(i)){
        return(i)
    }
    
    ## compute the inverse of matrix
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    
    i
}
