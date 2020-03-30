## This function creates a "special matrix" that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse (solve) of a matrix, unless its already beenm cached. Then it retrieves the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cashes data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
