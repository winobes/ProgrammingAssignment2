## The functions in this file support matrix inverse cacheing. We assume
## that all all matrices supplied to these functions are invertible.


## makeCacheMatrix creates a special "matrix" object which is really a list 
## contaning functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix inverse
##   4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

    x_inv <- NULL

    # sets the value of the matrix to y and clears its 
    # inverse (since the matirx itself is now changed)
    set <- function(y) {
        x <<- y
        x_inv <<- NULLA
    }
    
    # returns the matirx
    get <- function () x

    # sets the cached matrix inverse
    setinverse <- function(inv) x_inv <<- inv

    # gets the cached matrix inverse
    getinverse <- function() x_inv

    # return the special "matrix" object as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with 
## the above function. 

cacheSolve <- function(x, ...) {

    # get the value in the cache
    x_inv <- x$getinverse()
    # if its not NULL then the inverse is cached
    if(!is.null(x_inv)) {
        message("getting cached inverse")
        # return the cached inverse
        return(x_inv)
    }

    # otherwise calculate the inverse 
    x_matrix <- x$get()
    x_inv <- solve(x_matrix) # assumes that x_matrix is invertible!
    # store the inverse in the cache 
    x$setinverse(x_inv)
    # return the newly calculated inverse
    x_inv

}
