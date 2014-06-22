## The functions "makeCacheMatrix" and "cacheSolve" presented below, are part of a logic to cache the inverse of a matrix in memory.
## This is special useful for large matrices making the process of inverting such matrices much faster.
## A matrice is initially solved and the result is stored in memory, for subsequent uses it is retrieved from memory without the need to be inverted again.

## The code below is similar to the example functions provided for this assignment to calculate the mean of a vector where the function mean was used.

## To use these functions one should first invoke the function "makeCacheMatrix" with a squared matrix as a parameter, for example:
##      x <- makeCacheMatrix(matrix(c(3, 2, 5, 7), 2, 2))

## Using the function x$get() retrieves the initial matrix:

##              [,1] [,2]
##        [1,]    3    5
##        [2,]    2    7

## The function "cacheSolve" creates the inverse of a matrix if is not alrerady solved and returns the inverted matrix, for example:
## y <- cacheSolve(x), this create the inverse of x which is:
##      [,1]       [,2]
##      [1,]  0.6363636 -0.4545455
##      [2,] -0.1818182  0.2727273

## To confirm this is correct, the following multiplication should return the identity matrix:
##      y %*% x$get()
##      [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1

## To use thse functions correctly the input matrix needs to be a square matrix and it should be possible to invert it.

## This function returns a list of functions to deal with the cache of a matix and stores its value in memory. The matrix x and its inverse m are kept in memory for future uses.
## To access the original matrix the function get is used, and to get the inverse the getInverse function is used
## The function makeCacheMatrix defines four other functions:
##      set             - to setup a new matrix and stores its value into x. At the same time m is reset to invalidat any other values
##      get             - returns the original matrix (the one not inverted) that was stores on x
##      setInverse      - stores the inverse of the matrix x into m using the function solve
##      getInverse      - retrieves the inverse of matrix x that is stored into m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) m <<- solve
        
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The function "cacheSolve" is to be used in conjunction with the function "makeCacheMatrix" and it defines the inverse of a matrix, if it was not already done
## and returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        
        x$setInverse(m)
        
        m
}
