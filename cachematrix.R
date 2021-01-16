makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Creating Matrix 
  set <- function(y) {
    x <<- y ## set x to y
    m <<- NULL ## m to null
  }
  get <- function() x ## returns y
  setmat <- function(inverse) m <<- inverse ## inverse matrix
  getmat <- function() m ## get inverse matrix
  list(set = set, 
       get = get,
       setmat = setmat,
       getmat = getmat)
}


cacheSolve <- function(x, ...) {
  m <- x$getmat() ## getmat from makeCacheMatrix assigned to m
  if(!is.null(m)) { ##if m is not null
    message("getting cached data") ##message
    return(m) ## returning m (inverse matrix)
  }
  data <- x$get() ## get data x, to inverse
  m <- solve(data, ...) ## inverse
  x$setmat(m) 
  m ## return inverse of x
} 
