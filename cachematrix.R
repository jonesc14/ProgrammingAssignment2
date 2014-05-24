## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 'x' is a matrix that invertible
## Returns a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix(nrow=0, ncol=0)
        set <- function(y) {
                x <<- y
                i <<- matrix(nrow=0, ncol=0)
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
## 'x' is a special "matrix" object.  See makeCacheMatrix defined above.
## remaining arguments are passed to the function 'solve' to create the matrix inverse.
## Returns  a matrix that is the inverse of the matrix that was used when calling makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (length(i) != 0) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
