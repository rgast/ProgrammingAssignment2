makecacheMatrix <- function(x = matrix()) {  
        m <- NULL  
        set <- function(y) {  #sets value of matrix
                x <<- y
                m <<- NULL  
        }
        get <- function() x  # gets the matrix
        setinverse <- function(solve) m <<- solve  # sets inverse of matrix
        getinverse <- function() m  # returns the inverse matrix to cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {  #  computes the inverse of the special matrix 
        #  returned by makeCacheMatrix 
        m <- x$getinverse() # gets inverse matrix if already available
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if there is no cached inverse matrix, then compute inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
