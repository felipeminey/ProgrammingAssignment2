## Caching the inverse of a Matrix
## These functions are used to cache the inverse of a matrix the first time
## it is computed, so that further usage of the inverse matrix do not need
## to recalculate it.
## To effectively use these:
## 1. Call makeCacheMatrix passing your invertible matrix as parameter
## and storing the result into another variable.
## 
## 2. Call cacheSolve using the special matrix as a parameter.
## The iverse matrix will only be calculated once every time the matrix 
## changes. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse as NULL when the special matrix is created
    inv <- NULL
    ## whenever a new matrix ix SET, store the contents and reset the 
    ## inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## use get to retrieve the matrix contents 
    get <- function() x
    ## setinverse calculates the inverse matrix and store in inv
    setinverse <- function(solve) inv <<- solve
    ## getinverse returns whatever is stored in inv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a "special" matrix.
## If the inverse matrix was not calculated since the last change
## to the matrix, it will be calculated again, stored in the cache,
## and returned. 
## If the inverse matrix is already in the cache, a message indicating
## that it is being read from the cache is printed, and the inverse 
## is NOT recalculated.

cacheSolve <- function(x, ...) {
    ## get the stored inverse matrix
    inv <- x$getinverse()
    ## verify if the inverse already exists
    if(!is.null(inv)) {
        ## it exists! Retrieve from the cache
        message("getting cached data")
        ## return the retrieved value and exit the function. All done.
        return(inv)
    }
    ## If we got to this point, the inverse is NOT cached.
    ## retrieve the contents of the matrix to be inverted
    data <- x$get()
    ## calculate the inverse matrix
    inv <- solve(data, ...)
    ## Store the inverse in the cache
    x$setinverse(inv)
    ## return the inverse matrix
    inv        
}
