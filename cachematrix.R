## June 21, 2015, GT
## R Programming @Coursera
## Programming assignment 2, lexical scoping
## Caching the inverse of a matrix

## function creates a special matrix which can cache its inverse providing that it's invertibal.
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if(!is.null(inv)) {
                ## return inverse retrieved from cache
                inv
        }else{
                ## use R function to calculate the inverse and set inverse to x using x$setinv(inv)
                inv = solve(x$get())
                x$setinv(inv)
                inv
        }
}
