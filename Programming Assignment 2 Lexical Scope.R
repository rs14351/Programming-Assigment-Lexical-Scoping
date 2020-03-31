makeVector <- function(x = numeric()){
  m <- NULL
  set <- function(y) {
         x <<- NULL
  }
  getm <- function()x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
        
}

cachemean <-  function(x, ...){
        m <- x$getmean()
        if(!is.null(m)){
                message("getting cacheg data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
makeCacheMatrix <- function(x= matrix()){
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) <<- inverse
        getInverse <- function()j
        list(set = set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## End of attribution

cacheSolve <- function(x, ...){
## Here will return a matrix inverse of "x"
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
        return(J)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
j
        }
