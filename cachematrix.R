## This function creates a special vector, which is really a list containing 
## a function to 
##                1.set the value of the matrix
##                2.get the value of the matrix
##                3.set the value of the inverse
##                4.get the value of the inverse
## This function is the input of next function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of the input matrix of the previous function 
## The function will check if the inverse has already been calculated. If so, 
## gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse matrix and sets the value of the inverse in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

