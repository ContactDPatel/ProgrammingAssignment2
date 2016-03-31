# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here).


# set the value of the square matrix
# get the value of the square matrix
# set the value of the inverse of square marix using solve()
# get the value of the inverse of square matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of a square marix
# If iverse is already computed, it returns from cacheSolve
# Else compute inverse matrix, store it in cache and return the value


# ASSUMPTION: Supplied Matrix is ALWAYS invertible

cacheSolve <- function(x, ...) {
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
        message("getting cached data.")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data)
    x$setinverse(invmat)
    invmat
}
