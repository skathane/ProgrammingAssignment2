## The first function, makeCacheMatrix creates a special "matrix" object, 
## that can cache its inverse
## The second function computes the inverse of the special "matrix" above. 
## If the inverse has already been calculated, it retrieves the inverse from the cache.

## creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set the value of matrix
        setmatrix <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x        
        # set the value of matrix inverse
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        # create special matrix object that is a list
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## check if matrix inverse exists
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if not, compute matrix inverse and set in cache
        matrix <- x$getmatrix()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m        
}
