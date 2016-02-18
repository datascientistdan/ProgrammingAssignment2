## Lexical Scoping Assighment for R Programming
## with Johns Hopkins University
## Dan Johnson, 2/17/2016

## Cache the inverse of the matrix
## 4 Functions:
## get - returns vector in the main function
## set - changes the vector stored in the main function
## setmean
## getmean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Function to compute the inverse of the matrix
## Check to see if already cached
## If cached, usually local variable
## If not cached, create local cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

