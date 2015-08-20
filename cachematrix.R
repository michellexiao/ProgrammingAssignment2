## The functions below caches the inverse of a matrix


## makeCacheMatrix creates a list of functions(set, get, setinverse, getinverse)
## to cache the inverse of a matrix
## inv stores the inverse matrix of x
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ 
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinverse <-function(inverse) inv <<-inverse
        getinverse <-function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function does 2 things:
## First, if the inverse is already cached, it will retrieve the value from cache
## Second, if not, it will get the matrix, calculate the inverse, set the value into cache and return the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        } 
        data <- x$get() 
        inv <-solve(data,...) 
        x$setinverse(inv) 
        inv 
}
