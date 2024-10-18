## The following function creates a special matrix and calculates the inverse of this matrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via setinverse function.


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <-function(y) {
          x <<- y
          m <<- NULL
    }
    get <-function()x
    setinverse <- function(solve) m << - solve
    getinverse <- function() m
    list(set =set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## The following function calculates the inverse of this matrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value of the inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  
}
