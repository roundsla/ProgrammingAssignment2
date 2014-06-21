## The functions below create a special object to store the inverse of a
## given matrix so that it can be used again without time-consuming calculations.

## The makeCacheMatrix function creates an object to store or cache the results
## from calculating the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL ## initializes an object to store the inverse matrix
        set <- function(y) { ##sets the value of x and inverse_matrix in the parent environment
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x ## gets the value of x
        setinverse <- function(inverse) inverse_matrix <<- inverse ## sets the value of the inverse matrix
        getinverse <- function() inverse_matrix ## gets the value of the inverse matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function calls the cached value for the inverse of a matrix or performs the calculation if
## not available in the cache.

cacheSolve<-function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) { ## if the inverse matrix value returned is not 'NULL', its value will be returned
                message("getting cached data") 
        } 
        else { ## if the inverse matrix is not cached (a Null was returned), it will be calculated below
                data <- x$get() ## gets the value x
                inverse_matrix <- solve(data, ...) ## performs the 'solve' function to calculate the inverse matrix
                x$setinverse(inverse_matrix) ## caches the calculated inverse matrix
        }      
        inverse_matrix ## returns the inverse matrix value
}

