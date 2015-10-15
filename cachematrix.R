## The makeCacheMatrix class allows you to create matrix objects where each
## object has an additional property called 'inverse'. 
## The different functions for these objects allow you to manipulate 
## the original matrix data and its inverse.
## The cacheSolve function gets you inverse for a matrix of
## makeCacheMatrix class. If the inverse is stored in memory it simply retrieves
## it, otherwise it calculates the inverse and stores the inverse in memory.

## makeCacheMatrix takes a matrix as argument. It contains 'set', 'get'
## 'setinverse' and 'getinverse' functions to manipulate data for it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # set the matrix data
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # retrieves the matrix data
    get <- function() x
    
    # stores inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # retrieves inverse of the matrix
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an object made by 'makeCacheMatrix' and checks to see
## if its inverse is stored in memory. If so, it simply returns that. If not,
## it calculates the inverse and stores it in the memory.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Checks if the matrix has an inverse stored in memory
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message('getting cached inverse of the matrix...')
        return(inv)
    }
    
    # If inverse is not in memory, retrieve matrix data and calculate inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Stored calculated inverse in memory
    x$setinverse(inv)
    inv
}
