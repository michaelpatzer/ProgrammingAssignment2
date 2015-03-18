## Matrix inversion can be a costly computation and there may be benefit to caching
## the inverse of a matrix, rather than computing it more than once. This program provides
## two functions in order to cache the inverse of a matrix.
## To use this program (Sample run):
## 1. Create a matrix: x <- rbind(c(1, 5), c(-2, 3))
## 2. Create a matrix object: m <- makeCacheMatrix(x)
## 3. Display the matrix: m$get()
##      [,1] [,2]
## [1,]    1    5
## [2,]   -2    3
##
## 4. Run cacheSolve on the matrix object to return the inverse matrix: cacheSolve(m)
##           [,1]        [,2]
## [1,] 0.2307692 -0.38461538
## [2,] 0.1538462  0.07692308
##
## 5. Run cacheSolve again to demonstrate that it returns the cached value: cacheSolve(m)
## Using cached inverse.
##           [,1]        [,2]
## [1,] 0.2307692 -0.38461538
## [2,] 0.1538462  0.07692308

## makeCacheMatrix: Creates an object that caches an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    # Sets value for the matrix and resets the cached inverseMatrix
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Getter method returns the matrix
    get <- function() x
    
    # Setter & getter methods for inverse matrix
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    
    # Returns a list of functions available for this object
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: Returns the inverse of the object created with makeCacheMatrix. 
## If the result has already been cached, it will return the cached result, otherwise it will
## solve for the inverse of the matrix object and return that.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    
    # Return the cached inverse of the matrix if it exists.
    if (!is.null(inverseMatrix)) {
        message("Using cached inverse.")
        return(inverseMatrix)
    }
    
    # Otherwise, solve for the inverse of the matrix and return the result.
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
