## functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setreverM <- function(reverM) m <<- reverM
    getreverM <- function() m
    list(set = set, get = get, setreverM = setreverM, getreverM = getreverM)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getreverM()
    if(!all(is.na(m))) {
        message("getting cached data")
        return(x$getreverM())
    }
    data <- x$get()
    m <- solve(data)
    x$setreverM(m)
    m  ## Return a matrix that is the inverse of 'x'
}