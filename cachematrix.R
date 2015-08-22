## With the help of makeCacheMatrix we creat a special Matrix object that can 
## cache its inverse. cachesolve finction computes the inverse of the special
## matrix returned by the makeCacheMatrix. In case inversehas already been 
##calculated  then cachesolve function retrieves the inverse from the cache.

## makeCacheMatrix is a function with argument x which is coarced into a matrix. 
## where set is a function with argument y which assigns value of y to x and 
## sets i to null. get function fetches the value of x. seti function that takes 
## inverse as an argument and copies its value as i. geti fetches the value of i


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set, get = get,
             seti = seti,
             geti = geti)
    }



## cacheSolve function calculates the inverse of makeCacheMatrix but it first
## checks if the inverse was already calculated. In case mean was already been 
##calculated it gets the calue from the cache, otherwise it calculates the 
## inverse from the data and sets the value of the inverse in the cache using
##seti function.

cacheSolve <- function(x, ...) {
    i <- x$geti()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$seti(i)
    i
}
