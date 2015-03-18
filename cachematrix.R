## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing
## a function to:
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}

## The second function, cacheSolve, calculates the inverse of a special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)                
                
        } else {
                
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                return(m)
        }
}
