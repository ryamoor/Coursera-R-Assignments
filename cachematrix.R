##Author: Ryan Moore
##Date: 7/22/2016
##Put comments here that give an overall description of what your functions do

##makeCacheMatrix Description:
        ##makeCacheMatrix is a function that has the main goal of allowing an inverse to be cached.
        ##it returns a list of functions that can be used for that purpose.
                ##set() allows for redefining your initial matrix and make sure to reset the inverse if changed
                ##get() returns the current matrix
                ##setinverse() allows the user to input an already calculated inverse
                ##getinverse() returns what is currently being cached as the inverse


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


##cacheSolve Description
        ##returns the cached inverse that has already been calculated for the matrix 
        ## or solves for the inverse of the matrix.
        

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
