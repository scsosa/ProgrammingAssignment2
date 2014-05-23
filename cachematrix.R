## --------------------  makeCacheMatrix Function ---------------------------
##    This function returns a list of 4 functions: set, get, setinv and getinv
##          set - create new matrix in x - flag that an inverse has not be
##                computed
##          get - return the matrix currently in x
##          setinv - inverse the matrix
##          getinv - return the inverse of the matrix IF it has already been
##                   calculated once.  If not, it will return NULL

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y                           
      m <<- NULL
    }   ## end set
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)         ##  return a list of functions
      
}  ## end makeCacheMatix funtion


## ----------------------  cacheSolve Function --------------------------------
##
##    The cacheSolve function determines whether a matrix has previously been
##    inversed by checking the cache.  If so, the function returns the previously
##    calculated solution.  If not, the function inverses the matrix and caches
##    the solution (stores it).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()             ##   query the x vector's cache         
    if(!is.null(m)) {           ##   if there is a cache
      message("getting cached data") 
      return(m)                 ##   return the cache - don't need to continue
    }
    data <- x$get()             ##   if there's no cache
    m <- solve(data, ...)       ##   compute the inverse of the matrix
    x$setinv(m)                 ##   save the result back to x's cache
    m                           ##   return the result
  
  
}   ## end cacheSolve matrix
