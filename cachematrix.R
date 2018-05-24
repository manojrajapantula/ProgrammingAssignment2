## The functions makeCacheMatrix and Cache solve are for creating a special object that stores a matrix 
## and caches its mean

## MakeCacheMatrix is to create a special matrix objet that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        setinv <- function(y){
                inv <<- y
        }
        getinv <- function () inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## Checks whether inverse of a matrix is available and returns it from the cache. If inverse is not available 
## the function calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get()
        inv_mat <- solve(mat)
        x$setinv(inv_mat)
        inv_mat
}

