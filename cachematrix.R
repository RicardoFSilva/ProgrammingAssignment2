## Put comments here that give an overall description of what your
## functions do...

## Write a short comment describing this function

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

## Write a short comment describing this function

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

# source("cachematrix.R")
# x <- makeCacheMatrix(matrix(c(3, 2, 5, 6), 2, 2))
# y <- cacheSolve(x)
# inverse:  0.75,  -0.25, -0.625,  0.375

