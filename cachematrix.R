## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

## Enter a matrix object into the function. It will return a cache of the 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) mtx <<- solve
        getmatrix <- function() mtx
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mtx<-x$getmatrix()
        if(!is.null(mtx)){
                message("Getting Cached Data")
                return(mtx)
        }
        matrix<-x$get()
        mtx<-solve(matrix, ...)
        x$setmatrix(mtx)
        mtx
}
