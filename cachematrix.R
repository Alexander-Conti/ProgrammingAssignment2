## Programming Assignment 2: Lexical Scoping
## R Program created by Alexander Conti on 18/jun/2017
##
## This program defines a function that calculates the inverse of a matrix, but do it
## in a smart way. In other words, if the calculation for this matrix has already been
## done, the previous calculation is returned, otherwise the inversion is calculated for
## the first time and returned as the result of the function
##
## I also included a code at the end, exemplifying the utilization of the function

## This function returns a list with the functions to store and retrieve ("cashe") an
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The purpose of the function is to calculate the inverse of the matrix
## But it checks if it has already been calculated earlier
## If so, the previous calculation is returned, otherwise the invertion 
## is calculated and stored for future use
## As indicated, I am not checking if the matrix is inversible (I am assuming it)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                ## message("getting cached data") : kept it just as a comment
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx)
        x$setinv(inv)
        inv
}


## Here I include an exemple of utilization of the functions

myMatrix <- matrix(1:4,2,2) 	## this is my matrix to be inverted

myMatrixCache <- makeCacheMatrix (myMatrix) ## creates my list of caching functions for my Matrix

cacheSolve(myMatrixCache)   ## Calculates the inverse and returns it

cacheSolve(myMatrixCache)   ## This second time, it is not calculated, but returned from the cache

