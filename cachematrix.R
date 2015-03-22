## Put comments here that give an overall description of what your
## functions do

## this function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) inv <<- invert
    getinvert <- function() inv
    list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)
}


## this function assumes that the matrix is invertable.

cacheSolve <- function(x, ...) {
    inv <- x$getinvert()
    if(!is.null(inv)) {
        message("get cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinvert(inv)
    inv
}
