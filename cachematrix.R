## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        
    inv <- NULL
    set <- function(y) { ##set the value of the matrix
        x <<- y        ##caches the inputted matrix so that cacheSolve can check whetehr it has changed
        inv <<- NULL   ## sets the value of inverse matrix to null
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, ## set the matrix
         get=get, ## get the matrix
	 setinverse=setinverse, ## set the inverse
         getinverse=getinverse) ## get the inverse
     ##this list is used as the input to cacheSolve()
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
     inv <- x$getinverse()     ## if the inverse has already been calculated this gets it
    if(!is.null(inv)) {
    ## get it from the cache and skips the computation. 
        message("getting cached data.")
        return(inv)
    }
     ## otherwise, calculates the inverse 
    data <- x$get()
    inv <- solve(data)
    ## sets the value of the inverse in the cache via the setinv function.
    x$setinverse(inv)
    inv
}
