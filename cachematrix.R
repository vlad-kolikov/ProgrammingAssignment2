## The two functions below are used to create a special object that stores
## a numeric matrix and caches its inverse, and to calculate its inverse
## or to get it from cache if it has been already calculated.

## The function makeCacheMatrix creates a special "matrix", which is really
## a list containing functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invm <<- inverse
        getinverse <- function() invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special "matrix"
## created with the above function. However, it first checks via the
## getinverse function to see if the iverse has already been calculated and 
## matrix didn't change since last calculation: this is done by check of
## dimensions equality and matrix product equality to identity matrix.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                dimid <- identical(dim(inverse), dim(x$get()))
                mid <- identical(inverse %*% x$get(), diag(dim(inverse)[1]))
                if(dimid & mid) {
                        message("getting cached data")
                        return(inverse)
                }
                
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
