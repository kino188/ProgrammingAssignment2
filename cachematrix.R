## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinvs <- function(inverse) i <<- inverse
      getinvs <- function() i
      list(set = set, get = get,
           setinvs = setinvs,
           getinvs = getinvs)

}


## The following function calculates the inverse of the special "matrix" created 
#with the above function. However, it first checks to see if the inverse has already
#been calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
#the cache via the setinvs function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinvs()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinvs(i)
      i
}
