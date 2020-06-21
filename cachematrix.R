## The functions that will be created in this assignment aim to compute square matrix inversions and cache values. So, when you try to compute the inversion of a matrix that you have already used, R will search into the environment in which the values are cached and will be able to return the answer instead of computing everything again, saving, eventually, processing time.

## This is the function that creates the a "special matrix", returning a list of functions that will be used in the "cacheSolve()" function to either do the computation or just return the stored value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This is the function that takes the result of the previous one and uses as its argument. It will look up if the matrix has already been inverted. If so, R will just return the result, without doing any computation, otherwise the function will compute the inversion of the matrix and return it

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)){
        message('Getting cached data...')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv) 
    inv
}
