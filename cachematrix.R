## We want to cache the inverse of a matrix hence we create 2 functions. makeCacheMatrix which is basically a list of functions that allows us to set and get a matrix and its inverse

## Similar to the example given, we create one function which is essentially a list of functions operating on a given matrix.
## set function fixes the value of the matrix and sets its inverse to NULL
## get function returns the matrix
## setinv sets the inverse value for the matrix
## getinv returns the inverse value for the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This function basically calls the above function to find whether the inverse for this particular matrix has already been cached or not. If not, then it calculates the inverse and sets it. If yes, then it simply returns that value without doing any computation.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(x)
        x$setinv(i)
        i

}
