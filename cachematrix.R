## These pair of functions can cache the inverse of a matrix 
## by creating a square invertible matrix and storing it in cache

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # store the cached value to m
        # initalize m to NULL
        m <- NULL
        
        # create the matrix 
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # get the matrix value
        get <- function() x
        
        # store the inversed matrix m in cache
        setinverse <- function(inverse) m <<-inverse
        
        # get the inversed matrix from cache
        getinverse <- function() m
        
        # return the created function to the calling function
        list(set = set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolver computes the inverse of the special "matrix" returned by makeCacheMatrix.  
## If the inverse has already been calcuated (and the matrix has not changed), the the cachesolve 
## will retrieve the inverse from the cache.
## Otherwise, the matrix will be inverted.  This function also returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## get the inversed matrix from cache memory
        m <- x$getinverse()
        
        # return message to indicate the inversed matrix is in cache
        if(!is.null(m)) {
                message("getting cached inverted matrix")
                
                # display matrix in consol
                return(m)
        }
 
        #otherwise, retrieve the original matrix and store it in matrix
        matrix <- x$get()
        
        
        # If the matrix is not in square dimension, return error message.
        tryCatch({
                # Inverted the square matrix
                m <- solve(matrix, ...)
        }, error = function(e)  {
                # the matrix is not in square 
                message("Error:  Can not invert the matrix.  It is not in a square invertible matrix")
        }, finally = {
                # store the inverted matrix 
                x$setinverse(m)  
        })
 
        
        # Return the inverted matrix
        m
        
}
