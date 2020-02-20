## The functions makeCacheMatrix and cacheSolve are used to make an 
## object that can store a matrix and the value of its inverse and to
## calculate and store the inverse of that matrix.

## makeCacheMatrix creates a list that contains four functions
## set stores the value of the matrix passed to the makeCacheMatrix
## get returns the value of the stored matrix
## setinverse stores the value of the inverse of the matrix
## getinverse returns the value of the stored inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix objext created by 
## makeCacheMatrix. If the inverse of the matrix is already stored,
## cacheSolve returns the stored result. If the inverse has not been stored,
## the function calculates the inverse, and stores it in the matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
