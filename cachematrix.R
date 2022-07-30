## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function retrieves the inverse from the cache.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) Inv <<- inverse
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- inverse(data, ...)
        x$setInv(Inv)
        Inv
}
