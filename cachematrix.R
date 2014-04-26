## These functions return the inverse of a given matrix if it is cached and re-calculate it, save it and return it if it is not.

## makeCacheMatrix takes a square invertible matrix as its input and returns a list of four functions that give the 
## matrix itself, sets the value of the matrix, gives the inverse of the matrix, and sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x   
  setinv <- function(inv) m <<- inv   
  getinv <- function() m 
  list(set = set, get = get,   
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes the list returned by makeCacheMatrix. If the inverse of the matrix is cached already, it returns the inverse.
## If it is not, cacheSolve calculates the inverse and saves it in the list.

cacheSolve <- function(x, ...) {
  m <- x$getinv()         
  if(!is.null(m)) {    
    message("getting cached data") 
    return(m)               
  }
  data <- x$get()          
  m <- solve(data, ...)    
  x$setinv(m)             
  m                 
}
