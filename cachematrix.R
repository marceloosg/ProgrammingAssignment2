## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set matrix data
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get matrix data
        get <- function() x
        #cache inverse of matrix
        setinv <- function(inverse) m <<- inverse
        #retrieve cached inverse
        getinv <- function() m
        #return defined functions as a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Try to get cached inverse
        m <- x$getinv()
        if(!is.null(m)) {
                # If there is a cached value, retrieve it
                message("getting cached data")
                # and return it
                return(m)
        }
        #if there is no cached value, get matrix data
        data <- x$get()
        # evaluate its inverse
        m <- solve(data, ...)
        # set its value into cache
        x$setinv(m)
        # then return it  
        m
}
