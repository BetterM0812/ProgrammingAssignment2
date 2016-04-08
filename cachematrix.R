## Caching the inverse of a Matrix
## Create a funcion that creates a special "matrix" object that can cache its inverse
## i means inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL        
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <-function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function creates a special "matrix" returned by makeCacheMatrix above. If the inverse
##has already been calculated (and the matix has not changed), then the cachesolve should
##retrieve the inverse from the cache.
##m stands for "matrix"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        		message("getting cached matrix")
        		return(i)
        }
        m <- x$get()
        i <- solve(m,...)
        x$setinverse(i)
        i
}

