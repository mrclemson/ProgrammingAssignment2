## This function creates a special "matrix" object that caches its inverse
## It contains four functions as follows
## 1. set function sets the matrix and clears the inverse in cache
## 2. get function returns the matrix
## 3. setInv function sets the inverse
## 4. getInv function retrieves the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    
    ## set function sets the matrix, and as a result
    ## clears the cached inverse
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    
    ## get function returns the matrix
    get <- function() x
    
    ## setInv function sets the inverse
    setInv <- function(solve) matInv <<- solve
    
    ## getinv function gets the inverse from cache
    getInv <- function() matInv

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Try to retrieve the inverse from cache if it exists
        matInv <- x$getInv()
        if (!is.null(matInv)) {
            message("getting cached data")
            return(matInv)
        }
        
        ## Compute if the inverse doesn' exist and save to cache
        m <- x$get()
        matInv <- solve(m)
        x$setInv(matInv)
        matInv
}