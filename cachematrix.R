##Programming Assignment 2: Lexical Scoping
##Coursera R Programming - Module 2 Week 3
##makeCacheMatrix and Cashesolve are functions 
##that are intended to help solve calculations 
##that could require high load and memory to perform
##the calculations. 


##makeCacheMatrix: The main purpose of this function,
##is to create a special matrix with the ability to 
##cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##cacheSolve: uses the special matrix created 
##previously to do the calculation verification. 
##if the inverse has already been calculated, it 
##returns the inverse in cache, and if it has not 
##yet been calculated, it performs the calculation 
##through the resolution function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
} 
