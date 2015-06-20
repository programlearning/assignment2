makeCacheMatrix <- function(x = matrix()) {
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  get <- function() x
  setreverse<- function(reverse) elc <<-reverse
  getreverse <- function() elc
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
  
}

cacheSolve <- function(x, ...) {
  elc <- x$getreverse()
  if(!is.null(elc)){
    message("getting cached data")
    return(elc)
  }
  data <- x$get()
  elc <- reverse(data, ...)
  x$setreverse(elc)
  elc
}
