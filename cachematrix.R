## This function makeCacheMatrix returns a special matrix that is really 
## a list of functions that can be used by the function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    ## Returns a list of functions: get, setinv and getinv
    ## get returns the matrix that was passed to makeCacheMatrix
    ## setinv stores the inverse in the cache
    ## getinv gets the inverse from the cache
    inv <- NULL
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(get = get,
         setinv = setinv,
         getinv = getinv) # Returns the list of functions
}


## This function cacheSolve takes a list of functions created from a matrix
## using the function makeCacheMatrix and returns the inverse of that matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', where 'x' was created using
    ## the function makeCacheMatrix
    inv <- x$getinv()       # Tries to find the inverse in the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)         # Returns the inverse from the cache
    }
    data <- x$get()         # Gets the original matrix for inversion
    inv <- chol2inv(data, ...) # Calculates inverse from Choleski decomposition
    x$setinv(inv)           # Stores the inverse to the cache
    inv                     # Returns the inverse of the matrix
}
