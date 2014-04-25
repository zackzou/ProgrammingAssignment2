##sample code : cacheSolve(makeCacheMatrix(mym))
##Note: mym must be a square matrix, and mym must be a nonsingular matrix
##Otherwise will occure an error

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inv
## get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function cacheSolve calculates the inv of the special "matrix" created with the above function.
## However, it first checks to see if the inv has already been calculated. 
## If so, it gets the inv from the cache and skips the computation. 
## Otherwise, it calculates the inv of the data and sets the value of the inv in the cache 
## via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
