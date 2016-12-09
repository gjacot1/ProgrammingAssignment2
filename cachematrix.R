## Programming Assignment 2: Write a pair of functions that cache 
## the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its own inverse.
## This object is a list that contains a function that:
##      1. sets the value of the matrix
##      2. gets the value of the matrix
##      3. sets the value of the inverse
##      4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special
## "matrix" created by the makeCacheMatrix function.
## It first checks to see if the inverse has already been
## calculated. If it has, cacheSolve gets the inverse from the
## cache and skips over the computation. Otherwise, it calculcates
## the inverse of the matrix and sets the value of the inverse
## in the cache with the setinverse function and returns it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}

## The testCacheMatrix function tests the above two functions.
## It sets the special "matrix" object from an input matrix to
## a temporary variable "test". It then runs this object through
## the cacheSolve function twice: Once to cache the input with
## the makeCacheMatrix function, and again to demonstrate that
## the cacheSolve function properly returns the cached inverse
## of the input.

testCacheMatrix <- function(testmatrix) {
        test <- makeCacheMatrix(testmatrix)
        cacheSolve(test)
        message("reruning cacheSolve on test to retrive cached data")
        cacheSolve(test)
}

## This is the test matrix I will use as an input to testCacheMatrix

testmatrix <- matrix(
        c(1,6,3,3,7,5,0,9,4),
        nrow=3,
        ncol=3
)

## Run testCacheMatrix on "testmatrix" to test the makeCacheMatrix
## and cacheSolve functions.

testCacheMatrix(testmatrix)
