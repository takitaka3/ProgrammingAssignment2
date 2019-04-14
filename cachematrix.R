##<Caching the Inverse of a Matrix>
##Since matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse 
##of a matrix rather than compute it repeatedly, I wrote
##the following functions cache the inverse of a matrix.


##This function creates a special "matrix" object that can 
##cache its inverse, which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get, 
             setmatrix = setmatrix, 
             getmatrix  = getmatrix)
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), 
##then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                solve(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

